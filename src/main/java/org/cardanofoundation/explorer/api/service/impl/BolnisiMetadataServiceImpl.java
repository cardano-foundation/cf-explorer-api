package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;

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
import reactor.core.scheduler.Schedulers;

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
    String redisKey = getBolnisiMetadataKey(BOLNISI_METADATA_KEY + jsonMetadata.hashCode());
    // get medata from redis
    MetadataBolnisi metadataBolnisi = (MetadataBolnisi) redisTemplate.opsForValue().get(redisKey);
    // if metadata is not null, return metadata
    if (metadataBolnisi != null) {
      return metadataBolnisi;
    }

    // if metadata is null, get metadata from on-chain and off-chain
    metadataBolnisi = getMetadataBolnisi(jsonMetadata);
    // set metadata to redis with expire time is 200 seconds
    redisTemplate.opsForValue().set(redisKey, metadataBolnisi);
    redisTemplate.expire(redisKey, 1, TimeUnit.HOURS);
    return metadataBolnisi;
  }

  private MetadataBolnisi getMetadataBolnisi(String jsonMetadata) {
    MetadataBolnisi metadataBolnisi;
    metadataBolnisi = getOnChainMetadata(jsonMetadata);
    Map<String, List<Object>> offChainMetadata = getOffChainMetadata(metadataBolnisi);
    boolean isCidVerified = CidUtils.verifyCid(metadataBolnisi.getCid(),
                                               JsonUtil.getPrettyJson(offChainMetadata));
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
    List<CompletableFuture<Map<String, String>>> completableFutures = new ArrayList<>();
    metadataBolnisi.getWineryData()
        .forEach(wineryData ->
                     completableFutures.add(
                         callWebclient(publicKeyUrl, byte[].class, wineryData.getWineryId())
                             .map(bytes -> Map.of(wineryData.getWineryId(),
                                                  HexUtil.encodeHexString(bytes)))
                             .toFuture()
                             .exceptionally(ex -> {
                               log.error("Error while getting public key from external api", ex);
                               metadataBolnisi.setExternalApiAvailable(false);
                               metadataBolnisi.setCidVerified(false);
                               metadataBolnisi.setWineryData(null);
                               return null;
                             })));

    Map<String, String> wineryPkeyMap = completableFutures.stream()
        .map(CompletableFuture::join)
        .filter(map -> !CollectionUtils.isEmpty(map))
        .flatMap(map -> map.entrySet().stream())
        .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

    metadataBolnisi.getWineryData().forEach(wineryData -> {
      String pKeyOnChain = wineryPkeyMap.get(wineryData.getWineryId());
      boolean isPKeyVerified = pKeyOnChain != null &&
          removePrefixHexString(wineryData.getPublicKey()).equals(
              removePrefixHexString(pKeyOnChain));
      wineryData.setPKeyVerified(isPKeyVerified);
    });
  }

  /**
   * @param jsonMetadata
   * @return
   */
  private MetadataBolnisi getOnChainMetadata(String jsonMetadata) {
    MetadataBolnisi.MetadataBolnisiBuilder metadataBolnisiBuilder = MetadataBolnisi.builder();
    metadataBolnisiBuilder.isExternalApiAvailable(true);
    try {
      ObjectMapper objectMapper = new ObjectMapper();
      JsonNode metadataNode = objectMapper.readTree(jsonMetadata);
      // get value with key "cid"
      String cid = metadataNode.get("cid").asText();
      metadataBolnisiBuilder.cid(cid);
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
    } catch (Exception e) {
      metadataBolnisiBuilder.isCidVerified(false);
      log.error("Error while getting data from json", e);
    }
    return metadataBolnisiBuilder.build();
  }


  @SuppressWarnings("unchecked")
  public Map<String, List<Object>> getOffChainMetadata(MetadataBolnisi metadataBolnisi) {
    return callWebclient(offChainMetadataUrl, String.class, metadataBolnisi.getCid())
        .flatMap(actualOffChainURL ->
                     callWebclient(actualOffChainURL.replace("%2F", "/"), LinkedHashMap.class))
        .doOnSuccess(linkedHashMap -> {
          if (linkedHashMap == null || linkedHashMap.isEmpty()) {
            metadataBolnisi.setCidVerified(false);
            metadataBolnisi.setWineryData(null);
          }
        })
        .doOnError(throwable -> {
          log.error("Error while getting bolnisi off-chain metadata", throwable);
          metadataBolnisi.setExternalApiAvailable(false);
          metadataBolnisi.setCidVerified(false);
          metadataBolnisi.setWineryData(null);
        })
        .block();
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

  private String getBolnisiMetadataKey(String value) {
    return String.format("%s_%s", network, value).toUpperCase();
  }
}
