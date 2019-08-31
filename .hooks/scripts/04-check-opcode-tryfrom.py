#!/usr/bin/env python3

import subprocess
import json
import os


def main():
    # find the match expression
    # make sure all variants are accounted for
    # make sure all values return the correct variant
    process = subprocess.run([
        'rustup', 'run', 'nightly', 'rustc', '-Z', 'ast-json', 'src/opcode.rs'
    ],
                             capture_output=True,
                             check=True)

    ast = json.loads(process.stdout.decode('utf-8'))

    for item in ast['module']['items']:
        if item['ident']['name'] == 'OpCode':
            opcode_enum = item
            break
    else:
        raise Exception("Couldn't find OpCode enum in AST")

    node = item['node']
    assert node['variant'] == 'Enum'
    assert 'variants' in node['fields'][0]

    variants = [(variant['ident']['name'], value)
                for value, variant in enumerate(node['fields'][0]['variants'])]

    for item in ast['module']['items']:
        impl = item

        if item['node']['variant'] != 'Impl':
            continue

        trait = item['node']['fields'][4]['path']['segments'][-1]
        if trait['ident']['name'] != 'TryFrom':
            continue

        arg1 = trait['args']['fields'][0]['args'][0]['fields'][0]['node'][
            'fields'][1]['segments'][-1]
        if arg1['ident']['name'] != 'u8':
            print('not impl of TryFrom<u8>')
            continue

        break
    else:
        raise Exception("Couldn't find TryFrom<u8> impl for OpCode in AST")

    for token in impl['tokens']:
        if token['variant'] == 'Delimited':
            impl_block = token
            break
    else:
        raise Exception(
            "Couldn't find impl block of impl TryFrom<u8> for OpCode")

    for token in impl_block['fields'][2]:
        if token['variant'] == 'Delimited' and token['fields'][1] == 'Brace':
            fn_body = token
            break
    else:
        raise Exception("Couldn't find try_from function body in impl")

    for token in fn_body['fields'][2]:
        if token['variant'] == 'Delimited' and token['fields'][1] == 'Paren':
            ok_arg = token
            break
    else:
        raise Exception("Couldn't find Ok() arg in impl")

    assert token['fields'][2][0]['fields'][0]['kind']['fields'][0] == 'match'

    for token in ok_arg['fields'][2]:
        if token['variant'] == 'Delimited':
            match_body = token
            break
    else:
        raise Exception("Couldn't find match body")

    matched_opcodes = []

    tokens = match_body['fields'][2]
    shift = lambda: tokens.pop(0)
    while tokens:
        pattern = shift()['fields'][0]

        if pattern['kind']['variant'] != 'Literal':
            break

        value = int(pattern['kind']['fields'][0]['symbol'])

        assert shift()['fields'][0]['kind'] == 'FatArrow'

        assert shift()['fields'][0]['kind']['fields'][0] == 'OpCode'
        assert shift()['fields'][0]['kind'] == 'ModSep'

        variant = shift()['fields'][0]['kind']['fields'][0]

        assert shift()['fields'][0]['kind'] == 'Comma'

        matched_opcodes.append((variant, value))

    variants = set(variants)
    matched_opcodes = set(matched_opcodes)
    if variants != matched_opcodes:
        print('\nDifference in OpCode enum and TryFrom:')
        print('\nVariants:', variants - matched_opcodes or 'none')
        print('\nMatched Opcodes:', matched_opcodes - variants or 'none', '\n')
        exit(1)


if __name__ == '__main__':
    if 'AUTOHOOK_STAGED_FILES' in os.environ and 'src/opcode.rs' not in os.environ['AUTOHOOK_STAGED_FILES']:
        exit(0)
    main()
