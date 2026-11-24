cHash = ENCODE(cName).
cHash = MD5-DIGEST(cFileData).
cHash = SHA1-DIGEST(mBlob).
cHash = MESSAGE-DIGEST("SHA-256", cPassword).
cHash = MESSAGE-DIGEST("MD5", cFileData).
