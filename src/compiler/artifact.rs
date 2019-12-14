// use crate::compiler::value::Value;

// type Index = u32;
// const INDEX_SIZE: usize = std::mem::size_of::<Index>();

struct Artifact {
    // constant_table: Vec<Value>,
    code: Vec<u8>,
}

impl Into<Box<[u8]>> for Artifact {
    fn into(self) -> Box<[u8]> {
        // let mut bytes = Vec::new();

        // bytes.extend(
        //     (self.constant_table.len() as Index)
        //         .to_le_bytes()
        //         .into_iter(),
        // );

        // for constant in self.constant_table {
        //     let const_bytes: Vec<u8> = constant.into();
        //     let byte_len = (const_bytes.len() as Index).to_le_bytes();
        //     bytes.extend(byte_len.into_iter());
        //     bytes.extend(const_bytes);
        // }

        // bytes.into_boxed_slice()
        self.code.into_boxed_slice()
    }
}

// macro_rules! as_array {
//     ($len:expr, $iter:expr) => {{
//         let mut array = [0; $len];
//
//         for (i, n) in $iter.enumerate() {
//             array[i] = n;
//         }
//
//         array
//     }};
// }

impl From<Vec<u8>> for Artifact {
    fn from(bytes: Vec<u8>) -> Self {
        // let constant_table_len =
        //     Index::from_le_bytes(as_array!(INDEX_SIZE, bytes.drain(0..INDEX_SIZE)));
        // let constant_table = Vec::with_capacity(constant_table_len as usize);

        // for i in 0..constant_table_len {
        //     let byte_len = Index::from_le_bytes(as_array!(INDEX_SIZE, bytes.drain(0..INDEX_SIZE)));
        //     let value = Value::from(bytes.drain(0..byte_len));
        //     constant_table.push(value);
        // }

        Self {
            /*constant_table,*/ code: bytes,
        }
    }
}
