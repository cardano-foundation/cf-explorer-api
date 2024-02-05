package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;

import jakarta.annotation.PostConstruct;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.http.HttpStatusCode;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.web.reactive.function.client.WebClient;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi.LotData;
import org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi.MetadataBolnisi;
import org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi.WineryData;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxMetadataRepository;
import org.cardanofoundation.explorer.api.service.BolnisiMetadataService;
import org.cardanofoundation.explorer.api.util.CidUtils;
import org.cardanofoundation.explorer.api.util.JwsUtils;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.TxMetadata;
import org.cardanofoundation.ledgersync.common.util.HexUtil;
import org.cardanofoundation.ledgersync.common.util.JsonUtil;
import reactor.core.publisher.Mono;

@Service
@RequiredArgsConstructor
@Log4j2
public class BolnisiMetadataServiceImpl implements BolnisiMetadataService {

  private static final String BOLNISI_METADATA_KEY = "BOLNISI_METADATA:";
  private final TxMetadataRepository txMetadataRepository;
  private final WebClient webClient;
  private final RedisTemplate<String, Object> redisTemplate;
  @Value("${application.network}")
  private String network;
  @Value("${application.api.bolnisi.off-chain}")
  private String offChainMetadataUrl;
  @Value("${application.api.bolnisi.public-key}")
  private String publicKeyUrl;

  @Override
  public MetadataBolnisi getBolnisiMetadata(String jsonMetadata) {
    MetadataBolnisi metadataBolnisi = getOnChainMetadata(jsonMetadata);

    if(!metadataBolnisi.isOnChainMetadataValid()) {
      return metadataBolnisi;
    }

    Map<String, List<Object>> offChainMetadata = getOffChainMetadata(metadataBolnisi);
    boolean isCidVerified = CidUtils.verifyCid(metadataBolnisi.getCid(), JsonUtil.getPrettyJson(offChainMetadata));
    if (!isCidVerified) {
      return metadataBolnisi;
    }
    metadataBolnisi.setCidVerified(true);

    verifyPublicKey(metadataBolnisi);

    Map<String, WineryData> wineryDataMap = metadataBolnisi.getWineryData().stream()
        .collect(Collectors.toMap(WineryData::getWineryId, Function.identity()));

    offChainMetadata.forEach((key, value) -> {
      WineryData wineryData = wineryDataMap.get(key);

      if (wineryData != null) {
        List<LotData> lots = wineryData.getLots();
        for (int i = 0; i < lots.size(); i++) {
          boolean isSignatureVerified = wineryData.isPKeyVerified() &&
              JwsUtils.verifySignatureWithEd25519(
                  wineryData.getPublicKey(),
                  lots.get(i).getSignature(),
                  JsonUtil.getPrettyJson(value.get(i)));

          LotData lotData = lots.get(i);
          lotData.setOffChainData(value.get(i));
          lotData.setSignatureVerified(isSignatureVerified);
        }
        wineryData.setLots(lots);
        wineryDataMap.put(key, wineryData);
      }
    });

    metadataBolnisi.setWineryData(new ArrayList<>(wineryDataMap.values()));
    return metadataBolnisi;
  }

  @Override
  public WineryData getWineryData(String txHash, String wineryId) {
    List<TxMetadata> txMetadataList = txMetadataRepository.findAllByTxHash(txHash)
        .stream()
        .filter(txMetadata -> txMetadata.getKey().equals(BigInteger.valueOf(1904)))
        .collect(Collectors.toList());

    return txMetadataList.stream()
        .map(txMetadata -> getBolnisiMetadata(txMetadata.getJson()))
        .map(MetadataBolnisi::getWineryData)
        .flatMap(List::stream)
        .filter(wineryData -> wineryData.getWineryId().equals(wineryId))
        .findFirst()
        .orElse(null);
  }


  private void verifyPublicKey(MetadataBolnisi metadataBolnisi) {
    String publicKeyRedisKey = getRedisKey(BOLNISI_METADATA_KEY + publicKeyUrl);
    Map<String, String> pKeyRedisCachedMap = new HashMap<>();
    List<CompletableFuture<Map<String, String>>> completableFutures = new ArrayList<>();

    metadataBolnisi.getWineryData()
        .forEach(wineryData -> {
          String pKeyCached = (String) redisTemplate.opsForHash().get(publicKeyRedisKey,
                                                                      wineryData.getWineryId());
          if (pKeyCached != null) {
            pKeyRedisCachedMap.put(wineryData.getWineryId(), pKeyCached);
          } else {
            completableFutures.add(
                callWebclient(publicKeyUrl, byte[].class, wineryData.getWineryId())
                    .map(bytes -> Map.of(wineryData.getWineryId(),
                                         HexUtil.encodeHexString(bytes)))
                    .toFuture()
                    .exceptionally(ex -> {
                      log.error("Error while getting public key from external api", ex);
                      metadataBolnisi.setExternalApiAvailable(false);
                      metadataBolnisi.setCidVerified(false);
                      return null;
                    }));
          }
        });

    Map<String, String> wineryPkeyMap =
        completableFutures.stream()
            .map(CompletableFuture::join)
            .filter(map -> !CollectionUtils.isEmpty(map))
            .flatMap(map -> map.entrySet().stream())
            .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (a, b) -> a));

    wineryPkeyMap.putAll(pKeyRedisCachedMap);

    metadataBolnisi.getWineryData().forEach(wineryData -> {
      String pKeyOnChain = wineryPkeyMap.get(wineryData.getWineryId());
      redisTemplate.opsForHash().putIfAbsent(publicKeyRedisKey,
                                             wineryData.getWineryId(),
                                             pKeyOnChain);
      redisTemplate.expire(publicKeyRedisKey, 1, TimeUnit.DAYS);
      boolean isPKeyVerified = pKeyOnChain != null &&
          removePrefixHexString(wineryData.getPublicKey()).equals(
              removePrefixHexString(pKeyOnChain));
      wineryData.setPKeyVerified(isPKeyVerified);
    });
  }

  /**
   * Parses JSON metadata and constructs a MetadataBolnisi object based on the parsed data.
   * This method assumes that the input JSON contains a field named "st" which indicates the type of metadata.
   * If the "st" field has the value "georgianWine", the method proceeds to extract additional fields such as "cid" and "d".
   * The "d" field is expected to contain winery data, which is processed to create a list of WineryData objects.
   * Each WineryData object includes a list of LotData objects, which are constructed from the signatures present in the JSON.
   * If any exceptions occur during parsing or processing, the method logs the error and sets the appropriate flags on the MetadataBolnisi builder.
   *
   * @param jsonMetadata The JSON string containing the on-chain metadata.
   * @return A MetadataBolnisi object populated with the parsed data. If an exception occurs, the object will have its verification flags set to false.
   * @throws Exception If there is an issue with parsing the JSON or processing the extracted data.
   */
  private MetadataBolnisi getOnChainMetadata(String jsonMetadata) {
    MetadataBolnisi.MetadataBolnisiBuilder metadataBolnisiBuilder = MetadataBolnisi.builder();
    metadataBolnisiBuilder.isExternalApiAvailable(true);
    metadataBolnisiBuilder.isOnChainMetadataValid(true);
    try {
      ObjectMapper objectMapper = new ObjectMapper();
      JsonNode metadataNode = objectMapper.readTree(jsonMetadata);
      // get value with key "cid"
      String cid = metadataNode.get("cid").asText();
      metadataBolnisiBuilder.cid(cid);
      String st = metadataNode.get("st").asText();
      if (st.equals("georgianWine")) {
          List<WineryData> wineryDataList = new ArrayList<>();
        // for each wineryId in the metadataNode of key "d"
        metadataNode.get("d").fieldNames()
            .forEachRemaining(wineryId -> {
              // get wineryNode from the metadataNode of key "d" with wineryId
              JsonNode wineryNode = metadataNode.get("d").get(wineryId);
              List<LotData> lots = new ArrayList<>();

              // get signature from the wineryNode of key "s"
              if (wineryNode.get("s").isArray()) {
                // put all signatures into lots
                wineryNode.get("s").forEach(signature -> {
                  LotData lotData = LotData.builder()
                      .signature(removePrefixHexString(signature.asText()))
                      .build();
                  lots.add(lotData);
                });
              }

              WineryData wineryData = WineryData.builder()
                  .wineryId(wineryId)
                  .publicKey(removePrefixHexString(wineryNode.get("pk").asText()))
                  .header(removePrefixHexString(wineryNode.get("h").asText()))
                  .lots(lots)
                  .build();
              wineryDataList.add(wineryData);
            });
        metadataBolnisiBuilder.wineryData(wineryDataList);
      }

    } catch (Exception e) {
      metadataBolnisiBuilder.isCidVerified(false);
      metadataBolnisiBuilder.isOnChainMetadataValid(false);
      log.error("Error while getting data from json", e);
    }
    return metadataBolnisiBuilder.build();
  }


  @SuppressWarnings("unchecked")
  public Map<String, List<Object>> getOffChainMetadata(MetadataBolnisi metadataBolnisi) {
    if(StringUtils.isEmpty(metadataBolnisi.getCid())) {
      return null;
    }

    String offChainRedisKey = getRedisKey(BOLNISI_METADATA_KEY + offChainMetadataUrl);
    Object metadataRedisCached = redisTemplate
        .opsForHash()
        .get(offChainRedisKey, metadataBolnisi.getCid());

    if (metadataRedisCached != null) {
      return (Map<String, List<Object>>) metadataRedisCached;
    }

    Map<String, List<Object>> offChainMetadata =
        callWebclient(offChainMetadataUrl, String.class, metadataBolnisi.getCid())
            .flatMap(actualOffChainURL ->
                         callWebclient(actualOffChainURL.replace("%2F", "/"), LinkedHashMap.class))
            .doOnSuccess(linkedHashMap -> {
              if (CollectionUtils.isEmpty(linkedHashMap)) {
                metadataBolnisi.setCidVerified(false);
                metadataBolnisi.setWineryData(null);
              }
            })
            .onErrorComplete(throwable -> {
              log.error("Error while getting bolnisi off-chain metadata", throwable);
              metadataBolnisi.setExternalApiAvailable(false);
              metadataBolnisi.setCidVerified(false);
              metadataBolnisi.setWineryData(null);
              return true;
            })
            .block();

    if (offChainMetadata != null) {
      redisTemplate.opsForHash().put(offChainRedisKey, metadataBolnisi.getCid(), offChainMetadata);
      redisTemplate.expire(offChainRedisKey, 1, TimeUnit.DAYS);
    }

    return offChainMetadata;
  }

  private <T> Mono<T> callWebclient(String url, Class<T> clazz, Object... vars) {
    return webClient.get()
        .uri(url, vars)
        .acceptCharset(StandardCharsets.UTF_8)
        .retrieve()
        .onStatus(HttpStatusCode::is5xxServerError,
                  clientResponse -> clientResponse
                      .bodyToMono(String.class)
                      .flatMap(s -> {
                        if (s == null || s.isEmpty() || !s.contains("703")) {
                          return Mono.error(
                              new BusinessException(BusinessCode.EXTERNAL_API_IS_NOT_AVAILABLE));
                        } else {
                          return Mono.empty();
                        }
                      }))
        .onStatus(HttpStatusCode::is4xxClientError, clientResponse -> Mono.empty())
        .bodyToMono(clazz);
  }

  private String removePrefixHexString(String hexString) {
    return hexString.startsWith("0x") ? hexString.substring(2) : hexString;
  }

  private String getRedisKey(String value) {
    return String.format("%s_%s", network, value).toUpperCase();
  }

  @PostConstruct
  public void init() {
    String offChainMetaDataRedisKey = getRedisKey(BOLNISI_METADATA_KEY + offChainMetadataUrl);
    String publicKeyRedisKey = getRedisKey(BOLNISI_METADATA_KEY + publicKeyUrl);

    redisTemplate.delete(offChainMetaDataRedisKey);
    redisTemplate.delete(publicKeyRedisKey);
  }
}
