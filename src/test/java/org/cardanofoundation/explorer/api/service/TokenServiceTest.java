package org.cardanofoundation.explorer.api.service;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import org.springframework.core.task.TaskExecutor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.common.enumeration.TokenType;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.MaTxMintMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenMetadataResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenMintTxResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenVolumeAnalyticsResponse;
import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.repository.AddressRepository;
import org.cardanofoundation.explorer.api.repository.AddressTokenBalanceRepository;
import org.cardanofoundation.explorer.api.repository.AddressTokenRepository;
import org.cardanofoundation.explorer.api.repository.AggregateAddressTokenRepository;
import org.cardanofoundation.explorer.api.repository.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.MaTxMintRepository;
import org.cardanofoundation.explorer.api.repository.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.TxRepository;
import org.cardanofoundation.explorer.api.service.cache.TokenPageCacheService;
import org.cardanofoundation.explorer.api.service.impl.TokenServiceImpl;
import org.cardanofoundation.explorer.api.test.projection.AddressTokenProjectionImpl;
import org.cardanofoundation.explorer.api.test.projection.TokenNumberHoldersProjectionImpl;
import org.cardanofoundation.explorer.api.test.projection.TokenVolumeProjectionImpl;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.cardanofoundation.explorer.consumercommon.entity.AssetMetadata;
import org.cardanofoundation.explorer.consumercommon.entity.MaTxMint;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

@ExtendWith(MockitoExtension.class)
class TokenServiceTest {

  @Mock
  private MultiAssetRepository multiAssetRepository;
  @Mock
  private MaTxMintRepository maTxMintRepository;
  @Mock
  private AssetMetadataRepository assetMetadataRepository;
  @Mock
  private AddressTokenRepository addressTokenRepository;
  @Mock
  private AddressRepository addressRepository;
  @Mock
  private TxRepository txRepository;
  @Mock
  private AddressTokenBalanceRepository addressTokenBalanceRepository;
  @Mock
  private TokenMapper tokenMapper;
  @Mock
  private MaTxMintMapper maTxMintMapper;
  @Mock
  private AssetMetadataMapper assetMetadataMapper;
  @Mock
  private AggregateAddressTokenRepository aggregateAddressTokenRepository;
  @Mock
  private TokenPageCacheService tokenPageCacheService;
  @Mock
  private TaskExecutor taskExecutor;
  @InjectMocks
  private TokenServiceImpl tokenService;

  @Test
  void testFilterToken_WhenQueryEmptyAndCacheAvailable() throws Exception {
    // Setup
    final Optional<BaseFilterResponse<TokenFilterResponse>> cacheResponse = Optional.of(
        new BaseFilterResponse<>(new PageImpl<>(List.of(TokenFilterResponse.builder()
            .id(1746535L)
            .name("484f534b59")
            .policy("a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235")
            .volumeIn24h("38874867596070")
            .numberOfHolders(93233L)
            .metadata(TokenMetadataResponse.builder()
                .logo(
                    "https://storage.ex-c.sotatek.works/cardano-explorer-api-storage-token/mainnet/a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235484f534b59")
                .ticker("HOSKY")
                .url("https://hosky.io")
                .decimals(0)
                .build())
            .build()))));
    when(tokenPageCacheService.getTokePageCache(any(Pageable.class)))
        .thenReturn(cacheResponse);

    // Run the test
    final BaseFilterResponse<TokenFilterResponse> result = tokenService.filterToken(
        "", PageRequest.of(0, 1));

    // Verify the results
    assertNotNull(result);
    assertEquals(cacheResponse.get(), result);
  }

  @Test
  void testFilterToken_WhenQueryNotEmptyAndCacheAvailable() throws Exception {
    // Setup
    when(tokenPageCacheService.getTokePageCache(any(Pageable.class)))
        .thenReturn(Optional.of(
            new BaseFilterResponse<>(new PageImpl<>(List.of(TokenFilterResponse.builder()
                .id(0L)
                .name("484f534b59")
                .policy("a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235")
                .volumeIn24h("38874867596070")
                .numberOfHolders(93233L)
                .metadata(TokenMetadataResponse.builder()
                    .logo(
                        "https://storage.ex-c.sotatek.works/cardano-explorer-api-storage-token/mainnet/a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235484f534b59")
                    .ticker("HOSKY")
                    .url("https://hosky.io")
                    .decimals(0)
                    .build())
                .build())))));

    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235")
        .name("484f534b59")
        .nameView("nameView")
        .fingerprint("fingerprint")
        .build();

    final Page<MultiAsset> multiAssets = new PageImpl<>(List.of(multiAsset));

    when(multiAssetRepository.findAll(anyString(), any(Pageable.class)))
        .thenReturn(multiAssets);

    final AssetMetadata assetMetadata = AssetMetadata.builder()
        .id(0L)
        .subject("a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235484f534b59")
        .name("name")
        .description("description")
        .policy("policy")
        .ticker("HOSKY")
        .logo(
            "https://storage.ex-c.sotatek.works/cardano-explorer-api-storage-token/mainnet/a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235484f534b59")
        .url("https://hosky.io")
        .build();

    final List<AssetMetadata> assetMetadataList = List.of(assetMetadata);
    when(assetMetadataRepository.findBySubjectIn(any())).thenReturn(assetMetadataList);

    final TokenFilterResponse tokenFilterResponse = TokenFilterResponse.builder()
        .id(0L)
        .name("484f534b59")
        .policy("a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235")
        .build();

    when(tokenMapper.fromMultiAssetToFilterResponse(any()))
        .thenReturn(tokenFilterResponse);

    when(txRepository.findMinTxByAfterTime(any())).thenReturn(Optional.of(0L));

    when(addressTokenRepository.sumBalanceAfterTx(anyCollection(), eq(0L))).thenReturn(List.of(
        TokenVolumeProjectionImpl.builder()
            .ident(0L)
            .volume(BigInteger.valueOf(38874867596070L))
            .build()
    ));

    doAnswer(invocation -> {
      ((Runnable) invocation.getArguments()[0]).run();
      return null;
    }).when(taskExecutor).execute(any(Runnable.class));

    when(addressTokenBalanceRepository.countByMultiAssetIn(any())).thenReturn(List.of(
        TokenNumberHoldersProjectionImpl.builder()
            .ident(0L)
            .numberOfHolders(93233L)
            .build()
    ));
    when(addressTokenBalanceRepository.countAddressNotHaveStakeByMultiAssetIn(
        any())).thenReturn(List.of());

    final TokenMetadataResponse tokenMetadataResponse = new TokenMetadataResponse(
        "https://hosky.io", "HOSKY",
        0,
        "https://storage.ex-c.sotatek.works/cardano-explorer-api-storage-token/mainnet/a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235484f534b59",
        "description");

    when(assetMetadataMapper.fromAssetMetadata(any())).thenReturn(tokenMetadataResponse);

    // Run the test
    final BaseFilterResponse<TokenFilterResponse> result = tokenService.filterToken(
        "HOSKY", PageRequest.of(0, 1));

    // Verify the results
    assertEquals(1, result.getData().size());
    assertEquals(93233L, result.getData().get(0).getNumberOfHolders());
    assertEquals("38874867596070", result.getData().get(0).getVolumeIn24h());
  }

  @Test
  void testFilterToken_WhenQueryNotEmptyAndCacheNotAvailable() throws Exception {
    // Setup
    when(tokenPageCacheService.getTokePageCache(any(Pageable.class)))
        .thenReturn(Optional.empty());

    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235")
        .name("484f534b59")
        .nameView("nameView")
        .fingerprint("fingerprint")
        .build();

    final Page<MultiAsset> multiAssets = new PageImpl<>(List.of(multiAsset));

    when(multiAssetRepository.findAll(anyString(), any(Pageable.class)))
        .thenReturn(multiAssets);

    final AssetMetadata assetMetadata = AssetMetadata.builder()
        .id(0L)
        .subject("a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235484f534b59")
        .name("name")
        .description("description")
        .policy("policy")
        .ticker("HOSKY")
        .logo(
            "https://storage.ex-c.sotatek.works/cardano-explorer-api-storage-token/mainnet/a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235484f534b59")
        .url("https://hosky.io")
        .build();

    final List<AssetMetadata> assetMetadataList = List.of(assetMetadata);
    when(assetMetadataRepository.findBySubjectIn(any())).thenReturn(assetMetadataList);

    final TokenFilterResponse tokenFilterResponse = TokenFilterResponse.builder()
        .id(0L)
        .name("484f534b59")
        .policy("a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235")
        .build();

    when(tokenMapper.fromMultiAssetToFilterResponse(any()))
        .thenReturn(tokenFilterResponse);

    when(txRepository.findMinTxByAfterTime(any())).thenReturn(Optional.of(0L));

    when(addressTokenRepository.sumBalanceAfterTx(anyCollection(), eq(0L))).thenReturn(List.of(
        TokenVolumeProjectionImpl.builder()
            .ident(0L)
            .volume(BigInteger.valueOf(38874867596070L))
            .build()
    ));

    doAnswer(invocation -> {
      ((Runnable) invocation.getArguments()[0]).run();
      return null;
    }).when(taskExecutor).execute(any(Runnable.class));

    when(addressTokenBalanceRepository.countByMultiAssetIn(any())).thenReturn(List.of(
        TokenNumberHoldersProjectionImpl.builder()
            .ident(0L)
            .numberOfHolders(93233L)
            .build()
    ));
    when(addressTokenBalanceRepository.countAddressNotHaveStakeByMultiAssetIn(
        any())).thenReturn(List.of());

    final TokenMetadataResponse tokenMetadataResponse = new TokenMetadataResponse(
        "https://hosky.io", "HOSKY",
        0,
        "https://storage.ex-c.sotatek.works/cardano-explorer-api-storage-token/mainnet/a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235484f534b59",
        "description");

    when(assetMetadataMapper.fromAssetMetadata(any())).thenReturn(tokenMetadataResponse);

    // Run the test
    final BaseFilterResponse<TokenFilterResponse> result = tokenService.filterToken(
        "HOSKY", PageRequest.of(0, 1));

    // Verify the results
    assertEquals(1, result.getData().size());
    assertEquals(93233L, result.getData().get(0).getNumberOfHolders());
    assertEquals("38874867596070", result.getData().get(0).getVolumeIn24h());
  }

  @Test
  void testGetTokenDetail_WhenTokenFoundAndMetadataJsonIsNullAndTokenTypeIsFT() {
    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("d12d8c05c03484409f157917f21b323824d892130e4085006eaefc4a")
        .name("PARA0043")
        .nameView("PARA0043")
        .fingerprint("asset1kz0wkuzt8293x5jsz7tryyjdvs6mh7rcupf9nz")
        .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);

    // Configure TokenMapper.fromMultiAssetToResponse(...).
    final TokenResponse tokenResponse = new TokenResponse();
    tokenResponse.setTokenLastActivity(Timestamp.valueOf(LocalDateTime.of(2020, 1, 1, 0, 0, 0, 0)));

    when(tokenMapper.fromMultiAssetToResponse(any())).thenReturn(tokenResponse);

    when(txRepository.findMinTxByAfterTime(
        any())).thenReturn(Optional.of(0L));

    when(addressTokenRepository.sumBalanceAfterTx(multiAsset, 0L))
        .thenReturn(new BigInteger("10000"));

    when(addressTokenBalanceRepository.countAddressNotHaveStakeByMultiAsset(
        multiAsset)).thenReturn(Optional.of(100L));

    // Configure AddressTokenBalanceRepository.countStakeByMultiAsset(...).
    when(addressTokenBalanceRepository.countStakeByMultiAsset(multiAsset))
        .thenReturn(Optional.of(100L));

    // Configure AssetMetadataRepository.findFirstBySubject(...).
    final AssetMetadata metadata = new AssetMetadata();
    metadata.setId(0L);
    metadata.setSubject("subject");
    metadata.setName("name");
    metadata.setDescription("description");
    metadata.setPolicy("policy");
    final Optional<AssetMetadata> assetMetadata = Optional.of(metadata);

    when(assetMetadataRepository.findFirstBySubject(anyString())).thenReturn(assetMetadata);

    // Configure AssetMetadataMapper.fromAssetMetadata(...).
    final TokenMetadataResponse tokenMetadataResponse = new TokenMetadataResponse("url", "ticker",
        0, "logo", "description");

    when(assetMetadataMapper.fromAssetMetadata(metadata)).thenReturn(tokenMetadataResponse);

    // Configure MultiAssetRepository.getLastActivityTimeOfToken(...).
    final Timestamp timestamp = Timestamp.valueOf(LocalDateTime.of(2020, 1, 1, 0, 0, 0, 0));
    when(multiAssetRepository.getLastActivityTimeOfToken(multiAsset)).thenReturn(timestamp);

    when(maTxMintRepository.getTxMetadataNFTToken(anyString())).thenReturn(null);

    // Run the test
    final TokenResponse result = tokenService.getTokenDetail("tokenId");

    // Verify the results
    assertEquals(200, result.getNumberOfHolders());
    assertEquals("10000", result.getVolumeIn24h());
    assertEquals(TokenType.FT, result.getTokenType());
    assertNull(result.getMetadataJson());
    assertEquals(timestamp, result.getTokenLastActivity());
    assertEquals(tokenMetadataResponse, result.getMetadata());
  }

  @Test
  void testGetTokenDetail_WhenTokenFoundAndMetadataJsonNotContainVersionKeyAndTokenTypeIsNFT() {
    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("0495e7467b9f8285ef79fca99fe1ed85ca19faba5b7d4dd425c3d884")
        .name("456c657068616e7453656372657441766174617273323135")
        .nameView("ElephantSecretAvatars215")
        .fingerprint("asset109mk0p5zlrk2sd5qt93v602v7zjuzax07dga47")
        .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);

    // Configure TokenMapper.fromMultiAssetToResponse(...).
    final TokenResponse tokenResponse = new TokenResponse();
    tokenResponse.setTokenLastActivity(Timestamp.valueOf(LocalDateTime.of(2020, 1, 1, 0, 0, 0, 0)));

    when(tokenMapper.fromMultiAssetToResponse(any())).thenReturn(tokenResponse);

    when(txRepository.findMinTxByAfterTime(
        any())).thenReturn(Optional.of(0L));

    when(addressTokenRepository.sumBalanceAfterTx(multiAsset, 0L))
        .thenReturn(new BigInteger("10000"));

    when(addressTokenBalanceRepository.countAddressNotHaveStakeByMultiAsset(
        multiAsset)).thenReturn(Optional.of(100L));

    // Configure AddressTokenBalanceRepository.countStakeByMultiAsset(...).
    when(addressTokenBalanceRepository.countStakeByMultiAsset(multiAsset))
        .thenReturn(Optional.of(100L));

    // Configure AssetMetadataRepository.findFirstBySubject(...).
    final AssetMetadata metadata = new AssetMetadata();
    metadata.setId(0L);
    metadata.setSubject("subject");
    metadata.setName("name");
    metadata.setDescription("description");
    metadata.setPolicy("policy");
    final Optional<AssetMetadata> assetMetadata = Optional.of(metadata);

    when(assetMetadataRepository.findFirstBySubject(anyString())).thenReturn(assetMetadata);

    // Configure AssetMetadataMapper.fromAssetMetadata(...).
    final TokenMetadataResponse tokenMetadataResponse = new TokenMetadataResponse("url", "ticker",
        0, "logo", "description");

    when(assetMetadataMapper.fromAssetMetadata(metadata)).thenReturn(tokenMetadataResponse);

    // Configure MultiAssetRepository.getLastActivityTimeOfToken(...).
    final Timestamp timestamp = Timestamp.valueOf(LocalDateTime.of(2020, 1, 1, 0, 0, 0, 0));
    when(multiAssetRepository.getLastActivityTimeOfToken(multiAsset)).thenReturn(timestamp);

    when(maTxMintRepository.getTxMetadataNFTToken(anyString())).thenReturn(
        "{\"0495e7467b9f8285ef79fca99fe1ed85ca19faba5b7d4dd425c3d884\":{\"ElephantSecretAvatars215\":{\"image\":\"ipfs://QmNvjyj4o7p7UXMEbxnx9ZY5ZLFMJcy9sjMXaTiFRgC4nJ\",\"name\":\"ElephantSecretAvatars#0215\",\"files\":[{\"src\":\"ipfs://QmaRoAcJcHunUsEEE1FSiPm5SWe47PQbexwQpHZdbTLzde\",\"name\":\"ElephantSecretAvatars#0215\",\"mediaType\":\"model/gltf-binary\"}],\"Animation\":\"Idle\",\"Skin\":\"Pink\",\"mediaType\":\"image/png\"}}}");
    // Run the test
    final TokenResponse result = tokenService.getTokenDetail("tokenId");

    // Verify the results
    assertEquals(200, result.getNumberOfHolders());
    assertEquals("10000", result.getVolumeIn24h());
    assertEquals(
        "{\"0495e7467b9f8285ef79fca99fe1ed85ca19faba5b7d4dd425c3d884\":{\"ElephantSecretAvatars215\":{\"image\":\"ipfs://QmNvjyj4o7p7UXMEbxnx9ZY5ZLFMJcy9sjMXaTiFRgC4nJ\",\"name\":\"ElephantSecretAvatars#0215\",\"files\":[{\"src\":\"ipfs://QmaRoAcJcHunUsEEE1FSiPm5SWe47PQbexwQpHZdbTLzde\",\"name\":\"ElephantSecretAvatars#0215\",\"mediaType\":\"model/gltf-binary\"}],\"Animation\":\"Idle\",\"Skin\":\"Pink\",\"mediaType\":\"image/png\"}}}",
        result.getMetadataJson());
    assertEquals(TokenType.NFT, result.getTokenType());
    assertEquals(timestamp, result.getTokenLastActivity());
    assertEquals(tokenMetadataResponse, result.getMetadata());
  }

  @Test
  void testGetTokenDetail_WhenTokenFoundAndMetadataJsonContainsVersionKeyAndTokenTypeIsNFT() {
    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("2aec93fa65aaedaf2fc0aa46c3ace89c0c8e091ed5f39b8f8127e664")
        .name("50726f6d6973657332323339")
        .nameView("Promises2239")
        .fingerprint("asset1crku723fffqp4c6zmfmz8xc0cpwm6wugcwetmw")
        .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);

    // Configure TokenMapper.fromMultiAssetToResponse(...).
    final TokenResponse tokenResponse = new TokenResponse();
    tokenResponse.setTokenLastActivity(Timestamp.valueOf(LocalDateTime.of(2020, 1, 1, 0, 0, 0, 0)));
    tokenResponse.setTokenType(TokenType.NFT);

    when(tokenMapper.fromMultiAssetToResponse(any())).thenReturn(tokenResponse);

    when(txRepository.findMinTxByAfterTime(
        any())).thenReturn(Optional.of(0L));

    when(addressTokenRepository.sumBalanceAfterTx(multiAsset, 0L))
        .thenReturn(new BigInteger("10000"));

    when(addressTokenBalanceRepository.countAddressNotHaveStakeByMultiAsset(
        multiAsset)).thenReturn(Optional.of(100L));

    // Configure AddressTokenBalanceRepository.countStakeByMultiAsset(...).
    when(addressTokenBalanceRepository.countStakeByMultiAsset(multiAsset))
        .thenReturn(Optional.of(100L));

    // Configure AssetMetadataRepository.findFirstBySubject(...).
    final AssetMetadata metadata = new AssetMetadata();
    metadata.setId(0L);
    metadata.setSubject("subject");
    metadata.setName("name");
    metadata.setDescription("description");
    metadata.setPolicy("policy");
    final Optional<AssetMetadata> assetMetadata = Optional.of(metadata);

    when(assetMetadataRepository.findFirstBySubject(anyString())).thenReturn(assetMetadata);

    // Configure AssetMetadataMapper.fromAssetMetadata(...).
    final TokenMetadataResponse tokenMetadataResponse = new TokenMetadataResponse("url", "ticker",
        0, "logo", "description");

    when(assetMetadataMapper.fromAssetMetadata(metadata)).thenReturn(tokenMetadataResponse);

    // Configure MultiAssetRepository.getLastActivityTimeOfToken(...).
    final Timestamp timestamp = Timestamp.valueOf(LocalDateTime.of(2020, 1, 1, 0, 0, 0, 0));
    when(multiAssetRepository.getLastActivityTimeOfToken(multiAsset)).thenReturn(timestamp);

    when(maTxMintRepository.getTxMetadataNFTToken(anyString())).thenReturn(
        "{\"2aec93fa65aaedaf2fc0aa46c3ace89c0c8e091ed5f39b8f8127e664\":{\"Promises2239\":{\"Candidate\":\"RichardTrixson\",\"image\":\"ipfs://QmbWvwzLjwfKYqdbhar5KPzphAJQ1yZJ1xGbMi6C7A91CZ\",\"Series\":\"CampaignMaterials\",\"Promise\":\"Correct\",\"Number\":\"2239\",\"Banner\":\"Modern\",\"Asset\":\"Promises2239\",\"files\":[{\"src\":\"ipfs://QmbWvwzLjwfKYqdbhar5KPzphAJQ1yZJ1xGbMi6C7A91CZ\",\"name\":\"Promises2239\",\"mediaType\":\"image/jpeg\"},{\"src\":\"ipfs://QmTWuebKD7FC8kKh8tBoPsVedyhJg38g92dBopnb7fM98C\",\"name\":\"Promises2239\",\"mediaType\":\"image/jpeg\"}],\"Collection\":\"OldMoney\",\"mediaType\":\"image/jpeg\",\"Name\":\"Promises\"}},\"version\":\"1.0\"}");
    // Run the test
    final TokenResponse result = tokenService.getTokenDetail("tokenId");

    // Verify the results
    assertEquals(200, result.getNumberOfHolders());
    assertEquals("10000", result.getVolumeIn24h());
    assertEquals(TokenType.NFT, result.getTokenType());
    assertEquals(
        "{\"2aec93fa65aaedaf2fc0aa46c3ace89c0c8e091ed5f39b8f8127e664\":{\"Promises2239\":{\"Candidate\":\"RichardTrixson\",\"image\":\"ipfs://QmbWvwzLjwfKYqdbhar5KPzphAJQ1yZJ1xGbMi6C7A91CZ\",\"Series\":\"CampaignMaterials\",\"Promise\":\"Correct\",\"Number\":\"2239\",\"Banner\":\"Modern\",\"Asset\":\"Promises2239\",\"files\":[{\"src\":\"ipfs://QmbWvwzLjwfKYqdbhar5KPzphAJQ1yZJ1xGbMi6C7A91CZ\",\"name\":\"Promises2239\",\"mediaType\":\"image/jpeg\"},{\"src\":\"ipfs://QmTWuebKD7FC8kKh8tBoPsVedyhJg38g92dBopnb7fM98C\",\"name\":\"Promises2239\",\"mediaType\":\"image/jpeg\"}],\"Collection\":\"OldMoney\",\"mediaType\":\"image/jpeg\",\"Name\":\"Promises\"}},\"version\":\"1.0\"}",
        result.getMetadataJson());
    assertEquals(timestamp, result.getTokenLastActivity());
    assertEquals(tokenMetadataResponse, result.getMetadata());
  }

  @Test
  void testGetTokenDetail_WhenTokenNotFound() {
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(Optional.empty());
    assertThrows(BusinessException.class, () -> tokenService.getTokenDetail("tokenId"));
  }

  @Test
  void testGetMintTxs() {
    // Setup
    // Configure MaTxMintRepository.findByIdent(...).
    final MaTxMint maTxMint = new MaTxMint();
    maTxMint.setId(0L);
    final MultiAsset ident = new MultiAsset();
    ident.setPolicy("policy");
    ident.setName("name");
    ident.setNameView("nameView");
    maTxMint.setIdent(ident);
    final Page<MaTxMint> maTxMints = new PageImpl<>(List.of(maTxMint));
    when(maTxMintRepository.findByIdent(anyString(), any(Pageable.class)))
        .thenReturn(maTxMints);

    // Configure MaTxMintMapper.fromMaTxMintToTokenMintTx(...).
    final TokenMintTxResponse tokenMintTxResponse = new TokenMintTxResponse();
    tokenMintTxResponse.setTxHash("txHash");
    tokenMintTxResponse.setAmount("amount");
    tokenMintTxResponse.setTime(LocalDateTime.of(2020, 1, 1, 0, 0, 0));

    when(maTxMintMapper.fromMaTxMintToTokenMintTx(maTxMint)).thenReturn(tokenMintTxResponse);

    // Run the test
    final BaseFilterResponse<TokenMintTxResponse> result = tokenService.getMintTxs(
        "tokenId", PageRequest.of(0, 1));

    // Verify the results
    assertEquals(tokenMintTxResponse, result.getData().get(0));
  }

  @Test
  void testGetTopHolders_WhenTokenFound() {
    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("policy")
        .name("name")
        .nameView("nameView")
        .fingerprint("fingerprint")
        .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);

    // Configure AddressTokenBalanceRepository.findAddressAndBalanceByMultiAsset(...).
    when(addressTokenBalanceRepository.findAddressAndBalanceByMultiAsset(eq(multiAsset),
        any(Pageable.class))).thenReturn(new PageImpl<>(List.of(
        AddressTokenProjectionImpl.builder()
            .build()
    )));

    // Configure AddressRepository.findAddressByIdIn(...).
    final Address address = Address.builder()
        .id(0L)
        .address("address")
        .txCount(10L)
        .balance(new BigInteger("100"))
        .build();
    final List<Address> addresses = List.of(address);
    when(addressRepository.findAddressByIdIn(anyCollection())).thenReturn(addresses);

    // Configure TokenMapper.fromAddressTokenProjection(...).
    final TokenAddressResponse tokenAddressResponse = TokenAddressResponse.builder()
        .address(address.getAddress())
        .addressId(address.getId())
        .build();

    when(tokenMapper.fromAddressTokenProjection(any(AddressTokenProjection.class)))
        .thenReturn(tokenAddressResponse);

    // Run the test
    final BaseFilterResponse<TokenAddressResponse> result = tokenService.getTopHolders(
        "tokenId", PageRequest.of(0, 1));

    // Verify the results
    assertEquals(address.getAddress(), result.getData().get(0).getAddress());
  }

  @Test
  void testGetTopHolders_WhenTokenNotFound() {
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(Optional.empty());
    assertThrows(NoContentException.class,
        () -> tokenService.getTopHolders("tokenId", PageRequest.of(0, 1)));
  }

  @Test
  void testGetTokenVolumeAnalytic_WhenTokenFound_WithOneDayRange() throws Exception {
    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("policy")
        .name("name")
        .nameView("nameView")
        .fingerprint("fingerprint")
        .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);

    when(aggregateAddressTokenRepository.getMaxDay())
        .thenReturn(Optional.of(LocalDate.of(2020, 1, 1)));

    // Configure AddressTokenRepository.sumBalanceBetweenTx(...).
    when(addressTokenRepository.sumBalanceBetweenTx(any(MultiAsset.class),
        any(),
        any()))
        .thenReturn(Optional.of(new BigInteger("100")));

    // Run the test
    final List<TokenVolumeAnalyticsResponse> result = tokenService.getTokenVolumeAnalytic(
        "tokenId", AnalyticType.ONE_DAY);

    // Verify the results
    for (TokenVolumeAnalyticsResponse tokenVolumeAnalyticsResponse : result) {
      assertEquals(new BigInteger("100"), tokenVolumeAnalyticsResponse.getValue());
    }
  }

  @Test
  void testGetTokenVolumeAnalytic_WhenTokenFound_WithOneDayRangeAndExistsZeroBalance()
      throws Exception {
    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("policy")
        .name("name")
        .nameView("nameView")
        .fingerprint("fingerprint")
        .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);

    when(aggregateAddressTokenRepository.getMaxDay())
        .thenReturn(Optional.of(LocalDate.of(2020, 1, 1)));

    // Configure AddressTokenRepository.sumBalanceBetweenTx(...).
    when(addressTokenRepository.sumBalanceBetweenTx(any(MultiAsset.class),
        any(),
        any()))
        .thenReturn(Optional.of(new BigInteger("0")));

    when(addressTokenRepository.countRecord(any(MultiAsset.class), any(), any())).thenReturn(0L);

    // Run the test
    final List<TokenVolumeAnalyticsResponse> result = tokenService.getTokenVolumeAnalytic(
        "tokenId", AnalyticType.ONE_DAY);

    // Verify the results
    for (TokenVolumeAnalyticsResponse tokenVolumeAnalyticsResponse : result) {
      assertEquals(null, tokenVolumeAnalyticsResponse.getValue());
    }
  }

  @Test
  void testGetTokenVolumeAnalytic_WhenTokenFound_WithNotOneDayRange() throws Exception {
    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("policy")
        .name("name")
        .nameView("nameView")
        .fingerprint("fingerprint")
        .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);

    when(aggregateAddressTokenRepository.getMaxDay())
        .thenReturn(Optional.of(LocalDate.now().minusDays(3)));

    // Configure AddressTokenRepository.sumBalanceBetweenTx(...).
    when(aggregateAddressTokenRepository.sumBalanceInTimeRange(any(), any(),
        any())).thenReturn(Optional.of(new BigInteger("100")));

    lenient().when(addressTokenRepository.sumBalanceBetweenTx(any(MultiAsset.class),
            any(),
            any()))
        .thenReturn(Optional.of(new BigInteger("100")));

    // Run the test
    final List<TokenVolumeAnalyticsResponse> result = tokenService.getTokenVolumeAnalytic(
        "tokenId", AnalyticType.ONE_WEEK);

    // Verify the results
    for (TokenVolumeAnalyticsResponse tokenVolumeAnalyticsResponse : result) {
      assertEquals(new BigInteger("100"), tokenVolumeAnalyticsResponse.getValue());
    }
  }

  @Test
  void testGetTokenVolumeAnalytic_WhenTokenFound_WithNotOneDayRangeAndExistsZeroBalance()
      throws Exception {
    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("policy")
        .name("name")
        .nameView("nameView")
        .fingerprint("fingerprint")
        .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);

    when(aggregateAddressTokenRepository.getMaxDay())
        .thenReturn(Optional.of(LocalDate.now().minusDays(3)));

    // Configure AddressTokenRepository.sumBalanceBetweenTx(...).
    when(aggregateAddressTokenRepository.sumBalanceInTimeRange(any(), any(),
        any())).thenReturn(Optional.of(new BigInteger("100")));

    lenient().when(addressTokenRepository.sumBalanceBetweenTx(any(MultiAsset.class),
            any(),
            any()))
        .thenReturn(Optional.of(new BigInteger("0")));

    when(addressTokenRepository.countRecord(any(MultiAsset.class), any(), any())).thenReturn(10L);

    // Run the test
    final List<TokenVolumeAnalyticsResponse> result = tokenService.getTokenVolumeAnalytic(
        "tokenId", AnalyticType.ONE_WEEK);

    // Verify the results
    assertEquals(new BigInteger("0"), result.get(result.size() - 1).getValue());
  }

  @Test
  void testGetTokenVolumeAnalytic_WhenTokenNotFound() {
    when(multiAssetRepository.findByFingerprint("tokenId")).thenReturn(Optional.empty());
    assertThrows(NoContentException.class,
        () -> tokenService.getTokenVolumeAnalytic("tokenId", AnalyticType.ONE_DAY));
  }
}
