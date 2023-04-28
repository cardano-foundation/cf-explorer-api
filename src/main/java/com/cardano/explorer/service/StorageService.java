package com.cardano.explorer.service;

/**
 * StorageService interface for storage service
 */
public interface StorageService {

  /**
   * Upload file to storage
   * @param bytes bytes of file
   * @param filename filename
   */
  void uploadFile(byte[] bytes, String filename);

  /**
   * Download file from storage
   * @param fileName filename
   * @return bytes of file
   */
  byte[] downloadFile(String fileName);
}
