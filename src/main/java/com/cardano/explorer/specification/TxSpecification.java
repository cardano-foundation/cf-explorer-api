package com.cardano.explorer.specification;

import static org.springframework.data.jpa.domain.Specification.where;

import com.cardano.explorer.entity.Block;
import com.cardano.explorer.entity.Tx;
import com.cardano.explorer.model.request.TxFilterRequest;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
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
    return (root, query, cb) -> where(hasBlockId(request.getBlockNo()))
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

      Join<Tx, Block> txBlockJoin = root.join("block", JoinType.LEFT);

      return criteriaBuilder.equal(txBlockJoin.get("blockNo"), blockNo);
    };
  }
}
