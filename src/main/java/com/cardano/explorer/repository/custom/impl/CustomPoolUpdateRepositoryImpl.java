package com.cardano.explorer.repository.custom.impl;

import com.cardano.explorer.entity.PoolOwner;
import com.cardano.explorer.entity.PoolUpdate;
import com.cardano.explorer.entity.StakeAddress;
import com.cardano.explorer.repository.custom.CustomPoolUpdateRepository;
import javax.persistence.EntityManager;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Root;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class CustomPoolUpdateRepositoryImpl implements CustomPoolUpdateRepository {

  private final EntityManager entityManager;

  @Override
  public String findRewardAccountByPool(Long poolId) {
    CriteriaBuilder cb = entityManager.getCriteriaBuilder();
    CriteriaQuery<PoolUpdate> cq = cb.createQuery(PoolUpdate.class);
    Root<PoolUpdate> poolUpdateR = cq.from(PoolUpdate.class);
    Join<PoolUpdate, StakeAddress> stakeAddressJoin = poolUpdateR.join("rewardAddr");
    cq.where(cb.equal(poolUpdateR.get("poolHash").get("id"), poolId));
    cq.select(stakeAddressJoin.get("view"));
    Query query = entityManager.createQuery(cq);
    query.setFirstResult(0);
    query.setMaxResults(1);
    return (String) query.getSingleResult();
  }

  @Override
  public String findOwnerAccountByPool(Long poolId) {
    CriteriaBuilder cb = entityManager.getCriteriaBuilder();
    CriteriaQuery<PoolOwner> cq = cb.createQuery(PoolOwner.class);
    Root<PoolOwner> poolOwnerR = cq.from(PoolOwner.class);
    Join<PoolOwner, PoolUpdate> poolUpdateJoin = poolOwnerR.join("poolUpdate");
    Join<PoolOwner, StakeAddress> stakeAddressJoin = poolOwnerR.join("stakeAddress");
    cq.where(cb.equal(poolUpdateJoin.get("poolHash").get("id"), poolId));
    cq.select(stakeAddressJoin.get("view"));
    Query query = entityManager.createQuery(cq);
    query.setFirstResult(0);
    query.setMaxResults(1);
    return (String) query.getSingleResult();
  }
}
