package com.cardano.explorer.service;

public interface StorageService {

  void uploadFile(byte[] bytes, String filename);

  byte[] downloadFile(String fileName);

  void deleteFile(String fileName, String fileType);
}
