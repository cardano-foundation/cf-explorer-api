package org.cardanofoundation.explorer.api.repository.jooq;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.api.util.EntityUtil;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import org.jooq.DSLContext;
import org.jooq.Field;
import org.jooq.SortField;

import static org.jooq.impl.DSL.field;
import static org.jooq.impl.DSL.table;

@Repository
public class JOOQMultiAssetRepository {

  private final DSLContext dslContext;
  private final EntityUtil entityUtil;

  public JOOQMultiAssetRepository(DSLContext dslContext,
                                  @Value("${spring.jpa.properties.hibernate.default_schema}") String schema) {
    this.dslContext = dslContext;
    this.entityUtil = new EntityUtil(schema, MultiAsset.class);
  }

  public List<MultiAsset> getMultiAsset(Pageable pageable) {
    int offset = pageable.getPageNumber() * pageable.getPageSize();

    var query = dslContext.selectFrom(table(entityUtil.getTableName()));

    Sort sort = pageable.getSort();
    if (sort.isSorted()) {
      List<SortField<?>> sortFields = getSortFields(sort);
      query.orderBy(sortFields);
    }

    query.limit(pageable.getPageSize()).offset(offset);

    return query.fetchInto(MultiAsset.class);
  }

  private List<SortField<?>> getSortFields(Sort sort) {
    List<SortField<?>> sortFields = new ArrayList<>();

    for (Sort.Order order : sort) {
      String property = order.getProperty();
      Sort.Direction direction = order.getDirection();

      Field<?> field = field(entityUtil.getColumnField(property));

      SortField<?> sortField = direction.isAscending() ? field.asc() : field.desc();
      sortFields.add(sortField);
    }

    return sortFields;
  }
}
