package org.cardanofoundation.explorer.api.repository.impl;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import java.util.List;
import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;
import org.cardanofoundation.explorer.api.repository.CustomBlockRepository;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.cardanofoundation.explorer.consumercommon.entity.Block_;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)
@RequiredArgsConstructor
public class CustomBlockRepositoryImpl implements CustomBlockRepository {

  @PersistenceContext EntityManager entityManager;

  @Override
  @Transactional(readOnly = true, propagation = Propagation.REQUIRES_NEW)
  public List<Block> findByBlockNoAndSpecifiedOffset(Long blockNo, int limit, Direction direction) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Block> blockQuery = criteriaBuilder.createQuery(Block.class);
    Root<Block> blockRoot = blockQuery.from(Block.class);
    Predicate blockNoNotNull = criteriaBuilder.isNotNull(blockRoot.get(Block_.blockNo));
    Predicate epochNoNotNull = criteriaBuilder.isNotNull(blockRoot.get(Block_.epochNo));
    Predicate selectionCondition;
    Order orderByBlockNo;

    if (direction.equals(Direction.ASC)) {
      Predicate blockNoGreaterThanOrEqual =
          criteriaBuilder.greaterThanOrEqualTo(blockRoot.get(Block_.blockNo), blockNo);
      selectionCondition =
          criteriaBuilder.and(blockNoNotNull, epochNoNotNull, blockNoGreaterThanOrEqual);
      orderByBlockNo = criteriaBuilder.asc(blockRoot.get(Block_.blockNo));
    } else {
      Predicate blockNoLessThanOrEqual =
          criteriaBuilder.lessThanOrEqualTo(blockRoot.get(Block_.blockNo), blockNo);
      selectionCondition =
          criteriaBuilder.and(blockNoNotNull, epochNoNotNull, blockNoLessThanOrEqual);
      orderByBlockNo = criteriaBuilder.desc(blockRoot.get(Block_.blockNo));
    }

    blockQuery.select(blockRoot).where(selectionCondition).orderBy(orderByBlockNo);

    return entityManager.createQuery(blockQuery).setMaxResults(limit).getResultList();
  }
}
