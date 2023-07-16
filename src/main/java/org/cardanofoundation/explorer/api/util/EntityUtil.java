package org.cardanofoundation.explorer.api.util;

import java.lang.reflect.Field;
import java.util.List;

import jakarta.persistence.Column;
import jakarta.persistence.Table;

import org.cardanofoundation.explorer.consumercommon.entity.BaseEntity;

public class EntityUtil {

  private final String schema;

  private final Class<? extends BaseEntity> entityClass;

  public EntityUtil(String schema, Class<? extends BaseEntity> entityClass) {
    this.schema = schema;
    this.entityClass = entityClass;
  }

  /**
   * @return the table name of the entity with the schema prefix.
   */
  public String getTableName() {
    return schema + "." + entityClass.getAnnotation(Table.class).name();
  }

  /**
   * @param fieldName the entity field name
   * @return the column database field name of the entity.
   */
  public String getColumnField(String fieldName) {
    Field field = getFieldByName(fieldName);
    if (field != null) {
      return field.getAnnotation(Column.class).name();
    }
    return null;
  }

  /**
   * @param fieldName
   * @return the field of the entity with the given name.
   */
  private Field getFieldByName(String fieldName) {
    List<Field> fields = List.of(entityClass.getDeclaredFields());
    for (Field field : fields) {
      if (field.getName().equals(fieldName)) {
        return field;
      }
    }
    return null;
  }
}
