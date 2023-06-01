package org.cardanofoundation.explorer.api.service.impl;

import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.S3Object;
import com.amazonaws.services.s3.model.S3ObjectInputStream;
import java.io.IOException;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.service.StorageService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class StorageServiceImpl implements StorageService {

  private final AmazonS3 s3Client;
  @Value("${cloud.aws.s3.bucket.name}")
  private String bucketName;

  @Override
  public byte[] downloadFile(String fileName) {
    S3Object s3Object = s3Client.getObject(bucketName, fileName);
    S3ObjectInputStream inputStream = s3Object.getObjectContent();
    try {
      return inputStream.readAllBytes();
    } catch (IOException e) {
      e.printStackTrace();
      throw new BusinessException(BusinessCode.INTERNAL_ERROR);
    }
  }
}
