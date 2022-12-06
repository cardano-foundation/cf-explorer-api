package com.cardano.explorer.specification;

import static org.springframework.data.jpa.domain.Specification.where;

import com.cardano.explorer.model.request.TxFilterRequest;
import com.sotatek.cardano.common.entity.Block;
import com.sotatek.cardano.common.entity.Block_;
import com.sotatek.cardano.common.entity.MaTxOut;
import com.sotatek.cardano.common.entity.MultiAsset;
import com.sotatek.cardano.common.entity.Tx;
import com.sotatek.cardano.common.entity.TxOut;
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
    return (root, query, cb) -> where(hasBlockId(request.getBlockNo())
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
  public static Specification<Tx> hasBlockId(Integer blockNo) {
    return (root, query, criteriaBuilder) -> {
      if (Objects.isNull(blockNo)) {
        return null;
      }

      Join<Tx, Block> txBlockJoin = root.join(Tx_.BLOCK, JoinType.LEFT);

      return criteriaBuilder.equal(txBlockJoin.get(Block_.BLOCK_NO), blockNo);
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
      ListJoin<Tx, TxOut> txOutJoin = root.joinList(Tx_.TX_OUT_LIST, JoinType.INNER);
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
      ListJoin<Tx, TxOut> txOutJoin = root.joinList(Tx_.TX_OUT_LIST, JoinType.INNER);
      Join<TxOut, MaTxOut> maTxOutListJoin = txOutJoin.join("maTxOuts");
      Join<MultiAsset, MaTxOut> assetMaTxOutJoin = maTxOutListJoin.join("ident");
      return criteriaBuilder.equal(assetMaTxOutJoin.get("fingerprint"), tokenId);
    };
  }
}
