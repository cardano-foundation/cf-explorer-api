package org.cardanofoundation.explorer.api.service;

/**
 * StorageService interface for storage service
 */
public interface StorageService {
  /**
   * Download file from storage
   * @param fileName filename
   * @return bytes of file
   */
  byte[] downloadFile(String fileName);
}
