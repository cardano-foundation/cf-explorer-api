package com.cardano.explorer.specification;

import static org.springframework.data.jpa.domain.Specification.where;

import com.cardano.explorer.entity.Block;
import com.cardano.explorer.model.request.BlockFilterRequest;
import java.util.Collection;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

@Component
public final class BlockSpecification extends BaseSpecification<Block, BlockFilterRequest> {


  /**
   * Mapping filter condition from request to specification
   *
   * @param request filter condition
   * @return specification condition
   */
  @Override
  public Specification<Block> getFilter(BlockFilterRequest request) {
    return (root, query, cb) -> where(hasEpochNo(request.getEpochNo()))
        .toPredicate(root, query, cb);
  }

  /**
   * Create specification with epochNo condition
   *
   * @param epochNo value of epochNo condition
   * @return specification for epochNo equal condition
   */
  public static Specification<Block> hasEpochNo(Integer epochNo) {
    if (epochNo == null) {
      return null;
    }
    return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("epochNo"), epochNo);
  }

  /**
   * Create specification for block have id in a list
   *
   * @param blockIds value of list id
   * @return specification for id in list condition
   */
  public static Specification<Block> hasIdIn(Collection<Long> blockIds) {
    return (root, query, criteriaBuilder) -> (root.get("id").in(blockIds));
  }


}
