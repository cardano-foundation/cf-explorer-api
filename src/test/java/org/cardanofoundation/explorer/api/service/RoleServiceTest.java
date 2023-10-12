package org.cardanofoundation.explorer.api.service;

import java.util.HashMap;
import java.util.Map;

import javax.management.relation.Role;

import org.cardanofoundation.explorer.api.service.impl.RoleServiceImpl;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class RoleServiceTest {

  private RoleService roleService;

  @BeforeEach
  void setup(){
    roleService = new RoleServiceImpl();
  }

  @Test
  void getLimit_testFromRoleDescription() {
    Map<String, Map<String, Object>> roleDescription = new HashMap<>();
    Map<String, Object> description = new HashMap<>();
    description.put("reportLimitPer24Hours",10);
    roleDescription.put("ROLE_USER",description);
    int reportLimit = roleService.getReportLimit(roleDescription);
    Assertions.assertEquals(10,reportLimit);
  }

  @Test
  void getLimit_testGetFromNullRoleDescription(){
    int reportLimit = roleService.getReportLimit(null);
    Assertions.assertEquals(0,reportLimit);
  }
}
