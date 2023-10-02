package org.cardanofoundation.explorer.api.security.filter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.AllArgsConstructor;
import org.cardanofoundation.explorer.api.mapper.RoleConfigurationMapper;
import org.cardanofoundation.explorer.api.mapper.RoleFilterMapper;
import org.cardanofoundation.explorer.api.mapper.RoleFunction;
import org.cardanofoundation.explorer.api.security.utility.RoleConfiguration;
import org.cardanofoundation.explorer.common.utils.JwtUtils;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import javax.management.relation.Role;
import java.io.IOException;
import java.util.*;

@Component
@AllArgsConstructor
public class DynamicFilter extends OncePerRequestFilter {

    private final RoleConfiguration roleConfiguration;

    @Override
    public void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws IOException, ServletException {

        RoleFilterMapper roleConf = roleConfiguration.getRoleConfiguration();

        if (roleConf.getAuth().contains(request.getRequestURI())) {
            for (final RoleConfigurationMapper roleMapper : roleConf.getRoles()) {
                String roleKey = roleMapper.getName();
                Map<String, ? super Object> desc = roleMapper.getFunction().stream()
                        .filter(RoleFunction.class::isInstance)
                        .findAny().orElse(new RoleFunction("", null)).getDescription();

                if(null != desc){
                    request.setAttribute(roleKey, desc);
                }
            }

        } else {
            //"Filter NOT intercepted";m
        }

        filterChain.doFilter(request, response);
    }

}