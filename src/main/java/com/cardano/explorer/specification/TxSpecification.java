package com.cardano.explorer.specification;

import static org.springframework.data.jpa.domain.Specification.where;

import com.cardano.explorer.model.request.TxFilterRequest;
import com.sotatek.cardano.common.entity.AddressToken;
import com.sotatek.cardano.common.entity.AddressToken_;
import com.sotatek.cardano.common.entity.AddressTxBalance;
import com.sotatek.cardano.common.entity.Block;
import com.sotatek.cardano.common.entity.Block_;
import com.sotatek.cardano.common.entity.MultiAsset;
import com.sotatek.cardano.common.entity.MultiAsset_;
import com.sotatek.cardano.common.entity.Tx;
import com.sotatek.cardano.common.entity.TxOut_;
import com.sotatek.cardano.common.entity.Tx_;
import java.util.Objects;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.ListJoin;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

@Component
public final class TxSpecification extends BaseSpecification<Tx, TxFilterRequest> {

  /**
   * Mapping filter condition from request to specification
   *
   * @param request filter condition
   * @return specification condition
   */
  @Override
  public Specification<Tx> getFilter(TxFilterRequest request) {
    return (root, query, cb) -> where(hasBlockNo(request.getBlockNo())
        .and(hasBlockHash(request.getBlockHash()))
        .and(hasAddress(request.getAddress()))
        .and(hasToken(request.getTokenId())))
        .toPredicate(root, query, cb);
  }

  /**
   * Create specification with blockNo condition
   *
   * @param blockNo value of blockNo condition
   * @return specification for blockNo equal condition
   */
  public static Specification<Tx> hasBlockNo(Long blockNo) {
    return (root, query, criteriaBuilder) -> {
      if (Objects.isNull(blockNo)) {
        return null;
      }

      Join<Tx, Block> txBlockJoin = root.join(Tx_.BLOCK, JoinType.LEFT);

      return criteriaBuilder.equal(txBlockJoin.get(Block_.BLOCK_NO), blockNo);
    };
  }

  /**
   * Create specification with blockNo condition
   *
   * @param blockHash value of blockNo condition
   * @return specification for blockNo equal condition
   */
  public static Specification<Tx> hasBlockHash(String blockHash) {
    return (root, query, criteriaBuilder) -> {
      if (StringUtils.isEmpty(blockHash)) {
        return null;
      }

      Join<Tx, Block> txBlockJoin = root.join(Tx_.BLOCK, JoinType.LEFT);

      return criteriaBuilder.equal(txBlockJoin.get(Block_.HASH), blockHash);
    };
  }

  /**
   * Create specification with address condition
   *
   * @param address value of address
   * @return specification for address equal condition
   */
  public static Specification<Tx> hasAddress(String address) {
    return (root, query, criteriaBuilder) -> {
      if (StringUtils.isEmpty(address)) {
        return null;
      }
      ListJoin<Tx, AddressTxBalance> txOutJoin = root.joinList(Tx_.ADDRESS_TX_BALANCES, JoinType.INNER);
      return criteriaBuilder.equal(txOutJoin.get(TxOut_.ADDRESS), address);
    };
  }

  /**
   * Create specification with token id condition
   *
   * @param tokenId value of token id (fingerprint)
   * @return specification for token id equal condition
   */
  public static Specification<Tx> hasToken(String tokenId) {
    return (root, query, criteriaBuilder) -> {
      if (StringUtils.isEmpty(tokenId)) {
        return null;
      }
      query.distinct(true);
      ListJoin<Tx, AddressToken> tokenListJoin = root.joinList(Tx_.ADDRESS_TOKENS, JoinType.INNER);
      Join<AddressToken, MultiAsset> addressTokenMultiAssetJoin = tokenListJoin.join(AddressToken_.MULTI_ASSET);
      return criteriaBuilder.equal(addressTokenMultiAssetJoin.get(MultiAsset_.FINGERPRINT), tokenId);
    };
  }
}
