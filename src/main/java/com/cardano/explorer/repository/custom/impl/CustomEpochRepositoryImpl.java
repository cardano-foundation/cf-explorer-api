package com.cardano.explorer.repository.custom.impl;

import com.cardano.explorer.entity.Epoch;
import com.cardano.explorer.repository.custom.CustomEpochRepository;
import javax.persistence.EntityManager;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class CustomEpochRepositoryImpl implements CustomEpochRepository {

  private final EntityManager entityManager;

  @Override
  public Epoch findByCurrentEpochNo() {
    CriteriaBuilder cb = entityManager.getCriteriaBuilder();
    CriteriaQuery<Epoch> cq = cb.createQuery(Epoch.class);
    Root<Epoch> epochRoot = cq.from(Epoch.class);
    Subquery<Integer> sub = cq.subquery(Integer.class);
    Root<Epoch> subRoot = sub.from(Epoch.class);
    sub.select(cb.max(subRoot.get("no")));
    cq.where(cb.equal(epochRoot.get("no"), sub));
    return entityManager.createQuery(cq).getSingleResult();
  }
}
