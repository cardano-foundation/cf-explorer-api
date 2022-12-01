package com.cardano.explorer.specification;

import static org.springframework.data.jpa.domain.Specification.where;

import com.cardano.explorer.model.request.TxFilterRequest;
import com.sotatek.cardano.common.entity.Block;
import com.sotatek.cardano.common.entity.Block_;
import com.sotatek.cardano.common.entity.Tx;
import com.sotatek.cardano.common.entity.TxOut;
import com.sotatek.cardano.common.entity.TxOut_;
import com.sotatek.cardano.common.entity.Tx_;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.ListJoin;
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
        .or(hasOutputAddress(request.getAddress())))
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
      if (blockNo == null) {
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
  public static Specification<Tx> hasOutputAddress(String address) {
    return (root, query, criteriaBuilder) -> {
      if (address == null) {
        return null;
      }
      ListJoin<Tx, TxOut> txOutJoin = root.joinList(Tx_.TX_OUT_LIST, JoinType.INNER);
      return criteriaBuilder.equal(txOutJoin.get(TxOut_.ADDRESS), address);
    };
  }
}
