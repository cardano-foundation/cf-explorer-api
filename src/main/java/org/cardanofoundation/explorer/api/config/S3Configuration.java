package org.cardanofoundation.explorer.api.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

import com.amazonaws.auth.AWSStaticCredentialsProvider;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.client.builder.AwsClientBuilder;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;

@Configuration
public class S3Configuration {

  @Bean
  @Profile("!prod")
  public AmazonS3 amazonS3Clone(
      @Value("${cloud.aws.s3.endpoint}") String serviceEndpoint,
      @Value("${cloud.aws.s3.path-style.enabled}") Boolean pathStyleAccessEnabled,
      @Value("${cloud.aws.credentials.access-key}") String accessKey,
      @Value("${cloud.aws.credentials.secret-key}") String secretKey,
      @Value("${cloud.aws.region.static}") String region,
      @Value("${cloud.aws.s3.bucket.name}") String bucketName) {
    final AmazonS3 amazonS3 =
        AmazonS3ClientBuilder.standard()
            .withEndpointConfiguration(
                new AwsClientBuilder.EndpointConfiguration(serviceEndpoint, region))
            .withPathStyleAccessEnabled(pathStyleAccessEnabled)
            .withCredentials(
                new AWSStaticCredentialsProvider(new BasicAWSCredentials(accessKey, secretKey)))
            .build();

    if (!amazonS3.doesBucketExistV2(bucketName)) {
      amazonS3.createBucket(bucketName);
    }
    return amazonS3;
  }

  @Bean
  @Profile("prod")
  public AmazonS3 amazonS3(
      @Value("${cloud.aws.credentials.access-key}") String accessKey,
      @Value("${cloud.aws.credentials.secret-key}") String secretKey,
      @Value("${cloud.aws.region.static}") String region,
      @Value("${cloud.aws.s3.bucket.name}") String bucketName) {
    final AmazonS3 amazonS3 =
        AmazonS3ClientBuilder.standard()
            .withRegion(region)
            .withCredentials(
                new AWSStaticCredentialsProvider(new BasicAWSCredentials(accessKey, secretKey)))
            .build();

    if (!amazonS3.doesBucketExistV2(bucketName)) {
      amazonS3.createBucket(bucketName);
    }
    return amazonS3;
  }
}
