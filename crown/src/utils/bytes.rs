use std::any;

use bytes::Bytes;

use crate::utils::error::ConversionError;
use crate::{CrownError, Result};

pub trait ToBytes {
    fn to_bytes(&self) -> Result<Vec<u8>>;
}

impl<T: ToBytes> ToBytes for Vec<T> {
    fn to_bytes(&self) -> Result<Vec<u8>> {
        let mut bytes = Vec::new();

        for item in self.iter() {
            let item = item.to_bytes()?;
            bytes.extend(item);
        }

        Ok(bytes)
    }
}

pub trait ToBytesExt: ToBytes {
    /// size of `size`.
    fn to_n_bytes(&self, size: usize) -> Result<Vec<u8>>
    where
        Self: Sized;
    fn to_u64(&self) -> Result<u64>;
    fn as_bytes(&self) -> Result<Bytes>;
}

impl<T> ToBytesExt for T
where
    T: ToBytes,
{
    fn to_n_bytes(&self, size: usize) -> Result<Vec<u8>>
    where
        Self: Sized,
    {
        let mut data = T::to_bytes(self)?;

        if data.len() > size {
            return Err(ConversionError::TooBig(any::type_name::<T>().to_string()))?;
        }

        data.resize(size, 0);
        Ok(data)
    }

    fn to_u64(&self) -> Result<u64> {
        let bytes = T::to_bytes(self)?;
        Ok(u64::from_le_bytes(bytes.try_into().unwrap()))
    }
    fn as_bytes(&self) -> Result<Bytes> {
        let bytes = T::to_bytes(self)?;
        Ok(Bytes::from(bytes))
    }
}

impl ToBytes for u64 {
    fn to_bytes(&self) -> Result<Vec<u8>> {
        Ok(self.to_le_bytes().to_vec())
    }
}

impl<T: ToBytes, const SIZE: usize> ToBytes for [T; SIZE] {
    fn to_bytes(&self) -> Result<Vec<u8>> {
        let mut bytes = Vec::new();

        for item in self.iter() {
            let item = item.to_bytes()?;
            bytes.extend(item);
        }

        Ok(bytes)
    }
}

impl ToBytes for String {
    fn to_bytes(&self) -> Result<Vec<u8>> {
        Ok(self.bytes().chain(std::iter::once(0)).collect())
    }
}

impl ToBytes for [u8] {
    fn to_bytes(&self) -> Result<Vec<u8>> {
        let data = self.to_vec();
        Ok(data)
    }
}

impl ToBytes for &[u8] {
    fn to_bytes(&self) -> Result<Vec<u8>> {
        let data = self.to_vec();
        Ok(data)
    }
}

impl ToBytes for Vec<u8> {
    fn to_bytes(&self) -> Result<Vec<u8>> {
        Ok(self.clone())
    }
}

impl ToBytes for &str {
    fn to_bytes(&self) -> Result<Vec<u8>> {
        if let Ok(data) = self.as_bytes() {
            Ok(data.to_vec())
        } else {
            Err(CrownError::Unknown("ToBytes".to_string()))
        }
    }
}
