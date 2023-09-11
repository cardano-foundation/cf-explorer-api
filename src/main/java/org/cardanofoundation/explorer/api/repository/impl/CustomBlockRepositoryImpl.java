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
import org.cardanofoundation.explorer.consumercommon.entity.BaseEntity_;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
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
  public List<Block> findByBlockIdAndLimit(Long blockId, int limit, Direction direction) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Block> blockQuery = criteriaBuilder.createQuery(Block.class);
    Root<Block> blockRoot = blockQuery.from(Block.class);
    Predicate selectionCondition;
    Order orderByBlockId;

    if (direction.equals(Direction.ASC)) {
      selectionCondition =
          criteriaBuilder.greaterThanOrEqualTo(blockRoot.get(BaseEntity_.id), blockId);
      orderByBlockId = criteriaBuilder.asc(blockRoot.get(BaseEntity_.id));
    } else {
      selectionCondition =
          criteriaBuilder.lessThanOrEqualTo(blockRoot.get(BaseEntity_.id), blockId);
      orderByBlockId = criteriaBuilder.desc(blockRoot.get(BaseEntity_.id));
    }

    blockQuery.select(blockRoot).where(selectionCondition).orderBy(orderByBlockId);
    return entityManager.createQuery(blockQuery).setMaxResults(limit).getResultList();
  }
}
