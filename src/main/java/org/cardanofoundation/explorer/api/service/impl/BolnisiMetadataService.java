package org.cardanofoundation.explorer.api.service.impl;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.web.reactive.function.client.WebClient;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.cardanofoundation.explorer.api.common.enumeration.TypeTokenGson;
import org.cardanofoundation.explorer.api.config.aop.singletoncache.SingletonCall;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi.LotData;
import org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi.MetadataBolnisi;
import org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi.WineryData;
import org.cardanofoundation.explorer.api.util.CidUtils;
import org.cardanofoundation.explorer.api.util.JwsUtils;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.ledgersync.common.util.HexUtil;
import org.cardanofoundation.ledgersync.common.util.JsonUtil;
import reactor.core.publisher.Mono;

@Component
@RequiredArgsConstructor
@Log4j2
public class BolnisiMetadataService {

  private final WebClient webClient;

  @Value("${application.api.bolnisi.off-chain}")
  String offChainMetadataUrl;

  @Value("${application.api.bolnisi.public-key}")
  String publicKeyUrl;

  @SingletonCall(typeToken = TypeTokenGson.BOLNISI_METADATA, expireAfterSeconds = 200)
  public MetadataBolnisi getWineryData(String jsonMetadata) {
    MetadataBolnisi metadataBolnisi = getOnChainMetadata(jsonMetadata);
    Map<String, List<Object>> offChainMetadata = getOffChainMetadata(
        metadataBolnisi.getCid());
    boolean isCidVerified = CidUtils.verifyCid(metadataBolnisi.getCid(),
                                               JsonUtil.getPrettyJson(offChainMetadata));
    if (!isCidVerified) {
      return metadataBolnisi;
    }

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
                  removePrefixHexString(wineryData.getPublicKey()),
                  removePrefixHexString(lots.get(i).getSignature()),
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
    metadataBolnisi.setCidVerified(true);
    return metadataBolnisi;
  }


  private void verifyPublicKey(MetadataBolnisi metadataBolnisi) {
    List<CompletableFuture<Map<String, String>>> completableFutures = new ArrayList<>();
    metadataBolnisi.getWineryData()
        .forEach(wineryData ->
                     completableFutures.add(
                         callWebclient(publicKeyUrl, byte[].class, wineryData.getWineryId())
                             .map(bytes -> Map.of(wineryData.getWineryId(),
                                                  HexUtil.encodeHexString(bytes)))
                             .toFuture()));

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
    List<WineryData> wineryDataList = new ArrayList<>();
    String cid = null;
    try {
      ObjectMapper objectMapper = new ObjectMapper();
      JsonNode metadataNode = objectMapper.readTree(jsonMetadata);
      // get value with key "cid"
      cid = metadataNode.get("cid").asText();

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
                    .signature(signature.asText())
                    .build();
                lots.add(lotData);
              });
            }

            WineryData wineryData = WineryData.builder()
                .wineryId(wineryId)
                .publicKey(wineryNode.get("pk").asText())
                .header(wineryNode.get("h").asText())
                .lots(lots)
                .build();
            wineryDataList.add(wineryData);
          });
    } catch (JsonProcessingException e) {
      log.error("Error while getting data from json", e);
    }
    return MetadataBolnisi.builder()
        .cid(cid)
        .wineryData(wineryDataList)
        .build();
  }


  @SuppressWarnings("unchecked")
  public Map<String, List<Object>> getOffChainMetadata(String cid) {
    return callWebclient(offChainMetadataUrl, String.class, cid)
        .flatMap(actualOffChainURL ->
                     callWebclient(actualOffChainURL.replace("%2F", "/"), LinkedHashMap.class))
        .block();
  }

  private <T> Mono<T> callWebclient(String url, Class<T> clazz, Object... vars) {
    return webClient.get()
        .uri(url, vars)
        .acceptCharset(StandardCharsets.UTF_8)
        .retrieve()
        .onStatus(status -> status.is4xxClientError() || status.is5xxServerError(),
                  clientResponse -> Mono.error(
                      new BusinessException(BusinessCode.EXTERNAL_API_IS_NOT_AVAILABLE)))
        .bodyToMono(clazz);
  }

  private String removePrefixHexString(String hexString) {
    return hexString.startsWith("0x") ? hexString.substring(2) : hexString;
  }
}
