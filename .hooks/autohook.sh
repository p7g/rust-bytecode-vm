#!/usr/bin/env bash
# https://github.com/p7g/Autohook

echo() { builtin echo "[Autohook] $*"; }
echo_verbose() { if [ "$AUTOHOOK_VERBOSE" != '' ]; then echo "$@"; fi; }
echo_error() { >&2 echo "[ERROR] $*"; }
echo_debug() { if [ "$AUTOHOOK_DEBUG" != '' ]; then >&2 echo "[DEBUG] $*"; fi; }

repo_root() { git rev-parse --show-toplevel; }
hooks_dir() { builtin echo "$(repo_root)/.hooks"; }

hook_types=(
    "applypatch-msg" "commit-msg" "post-applypatch" "post-checkout"
    "post-commit" "post-merge" "post-receive" "post-rewrite"
    "post-update" "pre-applypatch" "pre-auto-gc" "pre-commit"
    "pre-push" "pre-rebase" "pre-receive" "prepare-commit-msg"
    "update"
)

install() {
    repo_root=$(repo_root)
    echo_debug "[install] found repo_root '$repo_root'"
    hooks_dir="$repo_root/.git/hooks"
    echo_debug "[install] found hooks_dir '$hooks_dir'"
    autohook_linktarget="../../.hooks/autohook.sh"
    for hook_type in "${hook_types[@]}"; do
        hook_symlink="$hooks_dir/$hook_type"
        if [ ! -e "$hook_symlink" ]; then
            ln -s "$autohook_linktarget" "$hook_symlink"
            echo_debug "[install] linked '$autohook_linktarget' to '$hook_symlink'"
        else
            echo_debug "[install] file exists at '$hook_symlink', not linking"
        fi
    done
    echo_debug '[install] done'
}

uninstall() {
    repo_root=$(repo_root)
    echo_debug "[uninstall] found repo_root '$repo_root'"
    hooks_dir="$repo_root/.git/hooks"
    echo_debug "[uninstall] found hooks dir '$hooks_dir'"
    autohook_linktarget='../../.hooks/autohook.sh'
    for hook_type in "${hook_types[@]}"; do
        link_path="$hooks_dir/$hook_type"
        if [ "$(readlink "$link_path")" = "$autohook_linktarget" ]; then
            echo_debug "[uninstall] '$link_path' points to autohook, deleting"
            rm "$link_path"
        fi
    done
    echo_debug '[uninstall] done'
}

link_script() {
    script_name=$1; hook_type=$2; extension=$3
    if [ "$script_name" == '' ] || [ "$hook_type" == '' ]; then
        echo_error 'Expected script name and hook type args to link'
        return 1
    fi
    source_path="../scripts/$script_name"
    target_path="$(hooks_dir)/$hook_type"
    if [ "$extension" != '' ]; then
        target_path="$target_path/$extension"
        source_path="../$source_path"
    fi
    mkdir -p "$target_path"
    ln -s "$source_path" "$target_path/$script_name"
    return $?
}

run_symlinks() {
    if ! [ -d "$1" ]; then
        echo_debug "[run_symlinks] directory not found '$1'"
        return
    fi

    autohook_fifo_dir=$(mktemp -d "${TMPDIR:-.}/autohook_fifo_XXXX") || {
        echo_error '[run_symlinks] failed to create temp fifo dir'
        return 1
    }
    echo_debug "[run_symlinks] made temp fifo dir '$autohook_fifo_dir'"
    autohook_stdout="$autohook_fifo_dir/stdout"
    autohook_stderr="$autohook_fifo_dir/stderr"
    if ! mkfifo "$autohook_stdout" || ! mkfifo "$autohook_stderr"; then
        echo_error '[run_symlinks] failed to create temp stderr or stdout fifo'
        rm "$autohook_stdout" "$autohook_stderr"
        rmdir "$autohook_fifo_dir"
        return 1
    fi
    tail -f -n +1 "$autohook_stdout" >&1 &
    tail -f -n +1 "$autohook_stderr" >&2 &

    script_files=()
    while IFS='' read -r script_file; do
        script_files+=("$script_file")
    done < <(find "$1" -maxdepth 1 \( -type f -o -type l \) | sort)

    hook_type=$2
    accumulator=$3
    number_of_symlinks="${#script_files[@]}"
    echo_debug "[run_symlinks] found $number_of_symlinks symlinks: '${script_files[*]}'"
    if [ "$number_of_symlinks" -eq 1 ] && [ "$(basename "${script_files[0]}")" == "*" ]; then
        echo_debug '[run_symlinks] only script file was "*", setting number_of_symlinks to 0'
        number_of_symlinks=0
    fi
    echo_verbose "Found $number_of_symlinks scripts"
    hook_exit_code=0
    if [ "$number_of_symlinks" -gt 0 ]; then
        echo_debug '[run_symlinks] had symlinks, running scripts'
        for file in "${script_files[@]}"; do
            echo_verbose "BEGIN $file"
            echo_debug "[run_symlinks] running '$file' with staged files '$accumulator'"
            result=$(
                2>&1 \
                AUTOHOOK_HOOK_TYPE="$hook_type" \
                AUTOHOOK_STAGED_FILES=$accumulator \
                AUTOHOOK_REPO_ROOT="$(repo_root)" \
                AUTOHOOK_STDOUT="$autohook_stdout" \
                AUTOHOOK_STDERR="$autohook_stderr" \
                "$file"
            )
            script_exit_code=$?
            if [ "$script_exit_code" != 0 ]; then
                echo_debug "[run_symlinks] script exited with $script_exit_code"
                hook_exit_code=$script_exit_code
                echo_error "$hook_type script '$file' yielded negative exit code $hook_exit_code"
                echo_error "Result:"
                echo_error "$result"
                builtin echo
            fi
            echo_verbose "FINISH $file"
        done
    fi
    rm -rf "$autohook_fifo_dir"

    if [ "$hook_exit_code" -ne 0 ]; then
        exit 1
    fi
}

run_hook() {
    hook_type="$1"
    if [ "$hook_type" = '' ]; then
        echo_error '[run-hook] Missing hook type argument'
        exit 1
    fi
    shift
    IFS=' ' read -r -a file_types <<< "$@"

    echo_debug "[run-hook] got hook type '$hook_type' and file types '${file_types[*]}'"

    if [ "${#file_types[@]}" -eq 0 ] || [ "${file_types[*]}" = '' ]; then
        echo_debug "[run-hook] no file types passed, running global $hook_type scripts"
        run_symlinks "$(hooks_dir)/$hook_type" "$hook_type"
    else
        echo_debug "[run-hook] running $hook_type hook for file types: ${file_types[*]}"
        for file_type in "${file_types[@]}"; do
            run_symlinks "$(hooks_dir)/$hook_type/$file_type" "$hook_type"
        done
    fi
}

usage() {
    cat <<USAGE
usage: $0 [-vh] <command> [<args>]

Options:
	-v	Enable verbose mode
	-h	Print this help message

Where <command> is one of:
	install
		Install autohook hooks in the current git repository's .git
		folder.
	uninstall
		Delete all autohook links in the git hooks directory.
	link-script <script-name> <hook-type> [<file-type>]
		Create a symbolic link from the current repository's
		hooks/scripts/<script-name> file to hooks/<hook-type>,
		or hooks/<hook-type>/<file-type> if a file type (extension) is
		specified.
	run-hook <hook-type> [<file-type>]
		Run all scripts for the given hook type. If the file type is not
		specified, all scripts in hooks/<hook-type> will be executed. If
		a file type is specified, all scripts in
		hooks/<hook-type>/<file-type>
	help
		Show this help message

Variables:
	AUTOHOOK_DEBUG
		Enable debug mode, which prints many logs.
	AUTOHOOK_VERBOSE
		Enable verbose mode. Same as -v option

USAGE
}

main() {
    calling_file=$(basename "$0")
    trap '[ "$(jobs -p)" != "" ] && { jobs -p | xargs kill; } >/dev/null' SIGINT SIGTERM EXIT
    echo_debug "called by '$calling_file'"
    if [ "$calling_file" == "autohook.sh" ]; then
        if [ "$1" = '-h' ]; then
            usage "$@"
            exit 0
        elif [ "$1" = '-v' ]; then
            AUTOHOOK_VERBOSE=1
            shift
        fi

        case "$1" in
            install)
                echo_debug 'installing'
                install
                exit $?
                ;;
            uninstall)
                echo_debug 'uninstalling'
                uninstall
                exit $?
                ;;
            link-script)
                echo_debug 'linking script'
                shift
                link_script "$@"
                exit $?
                ;;
            run-hook)
                echo_debug 'running hook'
                shift
                run_hook "$@"
                exit $?
                ;;
            help)
                echo_debug 'showing usage'
                usage "$@"
                exit 0
                ;;
            *)
                echo_error "Invalid command '$1'"
                usage "$@"
                exit 1
                ;;
        esac
    else
        repo_root=$(repo_root)
        hook_type=$calling_file
        symlinks_dir="$(hooks_dir)/$hook_type"
        echo_debug "[main] repo root: '$repo_root', hook type: '$hook_type' symlinks dir: '$symlinks_dir'"

        echo_verbose "Running global $hook_type scripts"
        run_symlinks "$symlinks_dir" "$hook_type"
        echo_verbose "Finished running global $hook_type scripts"

        staged_files=$(git diff --cached --name-only | rev | sort | rev)
        echo_debug "[main] staged files: '$staged_files'"
        accumulator=()
        last_extension=
        if [ "$staged_files" != '' ]; then
            for staged_file in $staged_files ''; do # empty string allows us to run scripts for the last extension too
                echo_debug "[main] staged file: '$staged_file'"
                current_extension=${staged_file##*.}
                echo_debug "[main] previous extension '$last_extension' -> current extension '$current_extension'"
                if [ "$current_extension" != "$last_extension" ]; then
                    echo_debug '[main] current extension is different from last extension'
                    if [ "$last_extension" != '' ]; then # don't trigger on the first loop
                        echo_debug '[main] last extension is not empty, running scripts'
                        echo_verbose "Running scripts for $hook_type with extension $last_extension"
                        run_symlinks "$symlinks_dir/$last_extension" "$hook_type" "$(IFS=$'\n'; builtin echo "${accumulator[*]}")"
                        echo_verbose "Finished running scripts for $hook_type with extension $last_extension"
                    fi
                    if [ "$staged_file" == '' ]; then # end of staged files, break
                        echo_debug '[main] reached end of staged files'
                        break
                    fi
                    last_extension=$current_extension
                    accumulator=()
                    echo_debug "[main] updating last extension to '$current_extension'"
                fi
                echo_debug "[main] appending '$staged_file' to accumulator"
                accumulator+=("$staged_file")
            done
        fi
        echo_debug '[main] finished'
    fi
}

main "$@"
