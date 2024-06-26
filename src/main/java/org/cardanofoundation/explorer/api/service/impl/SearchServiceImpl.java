package org.cardanofoundation.explorer.api.service.impl;

import java.util.Objects;
import java.util.Optional;

import lombok.RequiredArgsConstructor;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolInfoProjection;
import org.cardanofoundation.explorer.api.model.response.search.AddressSearchResponse;
import org.cardanofoundation.explorer.api.model.response.search.PoolSearchResponse;
import org.cardanofoundation.explorer.api.model.response.search.ScriptSearchResponse;
import org.cardanofoundation.explorer.api.model.response.search.SearchResponse;
import org.cardanofoundation.explorer.api.model.response.search.TokenSearchResponse;
import org.cardanofoundation.explorer.api.repository.explorer.DrepInfoRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.BlockRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHashRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.ScriptRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxRepository;
import org.cardanofoundation.explorer.api.service.SearchService;
import org.cardanofoundation.explorer.api.util.AddressUtils;
import org.cardanofoundation.explorer.common.entity.enumeration.ScriptType;
import org.cardanofoundation.explorer.common.entity.explorer.DRepInfo;
import org.cardanofoundation.explorer.common.entity.ledgersync.Block;
import org.cardanofoundation.explorer.common.entity.ledgersync.MultiAsset;
import org.cardanofoundation.explorer.common.entity.ledgersync.Script;

@Service
@RequiredArgsConstructor
public class SearchServiceImpl implements SearchService {

  private final EpochRepository epochRepository;
  private final BlockRepository blockRepository;
  private final TxRepository txRepository;
  private final MultiAssetRepository multiAssetRepository;
  private final PoolHashRepository poolHashRepository;
  private final StakeAddressRepository stakeAddressRepository;
  private final ScriptRepository scriptRepository;
  private final DrepInfoRepository drepInfoRepository;

  @Value("${application.network}")
  private String network;

  @Override
  public SearchResponse search(String query) {
    String rawQuery = query;
    query = query.trim().toLowerCase();
    SearchResponse searchResponse = new SearchResponse();
    searchEpoch(query, searchResponse);
    searchBlock(query, searchResponse);
    searchTx(query, searchResponse);
    searchToken(query, searchResponse);
    searchAddress(rawQuery, searchResponse);
    searchPool(query, searchResponse);
    searchScriptHash(query, searchResponse);
    searchDRep(query, searchResponse);
    return searchResponse;
  }

  private void searchDRep(String query, SearchResponse searchResponse) {
    Optional<DRepInfo> dRepInfo = drepInfoRepository.findByDRepHashOrDRepId(query);
    dRepInfo.ifPresent(repInfo -> searchResponse.setDRep(repInfo.getDrepId()));
  }

  private void searchEpoch(String query, SearchResponse searchResponse) {
    try {
      Integer epochNo = Integer.parseInt(query);
      var epoch = epochRepository.findFirstByNo(epochNo);
      if (epoch.isPresent()) {
        searchResponse.setEpoch(epochNo);
      }
    } catch (NumberFormatException e) {
      searchResponse.setEpoch(null);
    }
  }

  private void searchBlock(String query, SearchResponse searchResponse) {
    Optional<Block> block;
    try {
      Long blockNo = Long.parseLong(query);
      block = blockRepository.findFirstByBlockNo(blockNo);
    } catch (NumberFormatException e) {
      block = blockRepository.findFirstByHash(query);
    }
    if (block.isPresent()) {
      searchResponse.setBlock(query);
    }
  }

  private void searchTx(String query, SearchResponse searchResponse) {
    var tx = txRepository.findByHash(query);
    if (tx.isPresent()) {
      searchResponse.setTx(query);
    }
  }

  private void searchToken(String query, SearchResponse searchResponse) {
    var token = multiAssetRepository.findByFingerprint(query);
    if (token.isPresent()) {
      searchResponse.setToken(
          new TokenSearchResponse(token.get().getNameView(), token.get().getFingerprint()));
    } else {
      Pageable pageable = PageRequest.of(0, 2);
      var tokenList = multiAssetRepository.findByNameViewLike(query, pageable);
      if (tokenList.size() == 1) {
        MultiAsset multiAsset = tokenList.get(0);
        searchResponse.setToken(
            new TokenSearchResponse(multiAsset.getNameView(), multiAsset.getFingerprint()));
      }
      if (!CollectionUtils.isEmpty(tokenList)) {
        searchResponse.setValidTokenName(true);
      }
    }
  }

  private void searchAddress(String query, SearchResponse searchResponse) {
    if (query.startsWith(CommonConstant.STAKE_ADDRESS_PREFIX)) {
      var stakeAddress = stakeAddressRepository.findByView(query);
      stakeAddress.ifPresent(
          address ->
              searchResponse.setAddress(new AddressSearchResponse(address.getView(), false, true)));
    } else {
      final int ADDRESS_MIN_LENGTH = 56;
      if (query.length() < ADDRESS_MIN_LENGTH) {
        return;
      }
      try {
        if (checkNetworkAddress(query)) {
          AddressUtils.checkStakeAddress(query);
          searchResponse.setAddress(new AddressSearchResponse(query, true, false));
        }
      } catch (Exception e) {
        // ignore
      }
    }
  }

  /**
   * Check address is valid in this network
   *
   * @param address address view value
   * @return true if valid and false if not
   */
  private boolean checkNetworkAddress(String address) {
    if (address.startsWith(CommonConstant.TESTNET_ADDRESS_PREFIX)) {
      return !network.equals(CommonConstant.MAINNET_NETWORK);
    } else {
      return network.equals(CommonConstant.MAINNET_NETWORK);
    }
  }

  private void searchPool(String query, SearchResponse searchResponse) {
    var pool = poolHashRepository.getPoolInfo(query);
    if (Objects.nonNull(pool)) {
      searchResponse.setPool(
          new PoolSearchResponse(pool.getPoolName(), pool.getPoolView(), pool.getIcon()));
    } else {
      Pageable pageable = PageRequest.of(0, 2);
      var poolList = poolHashRepository.findByPoolNameLike(query, pageable);
      if (poolList.size() == 1) {
        PoolInfoProjection poolInfo = poolList.get(0);
        searchResponse.setPool(
            new PoolSearchResponse(
                poolInfo.getPoolName(), poolInfo.getPoolView(), poolInfo.getIcon()));
      }
      if (!CollectionUtils.isEmpty(poolList)) {
        searchResponse.setValidPoolName(true);
      }
    }
  }

  public void searchScriptHash(String query, SearchResponse searchResponse) {
    Script script = scriptRepository.findByHash(query).orElse(null);

    if (Objects.nonNull(script)) {
      boolean isSmartContract =
          ScriptType.PLUTUSV1.equals(script.getType())
              || ScriptType.PLUTUSV2.equals(script.getType())
              || ScriptType.PLUTUSV3.equals(script.getType());

      ScriptSearchResponse scriptSearchResponse =
          ScriptSearchResponse.builder().scriptHash(script.getHash()).build();

      if (isSmartContract) {
        scriptSearchResponse.setSmartContract(true);
      } else {
        scriptSearchResponse.setNativeScript(true);
      }
      searchResponse.setScript(scriptSearchResponse);
    }
  }
}
