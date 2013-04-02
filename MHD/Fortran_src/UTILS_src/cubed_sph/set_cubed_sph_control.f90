!
!      module set_cubed_sph_control
!
!        programmed by H.Matsui on Apr., 2006
!
!      subroutine set_shell_paramteres
!
      module set_cubed_sph_control
!
      use m_precision
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_shell_paramteres
!
      use m_numref_cubed_sph
      use m_cubed_sph_radius
      use m_cubed_sph_grp_param
      use m_control_data_cubed_sph
!
      integer(kind = kint) :: i, j, k, jst, jed, kst, ked
      integer(kind = kint) :: if_CMB, if_ICB, if_EXT
!
!
!      if     (domain_shape_ctl .eq. 'sphere'                           &
!     &   .or. domain_shape_ctl .eq. 'Sphere'                           &
!     &   .or. domain_shape_ctl .eq. 'SPHERE') then
!        iflag_domain_shell = 1
!      else if(domain_shape_ctl .eq. 'spherical_shell'                  &
!     &   .or. domain_shape_ctl .eq. 'Spherical_shell'                  &
!     &   .or. domain_shape_ctl .eq. 'Spherical_Shell'                  &
!     &   .or. domain_shape_ctl .eq. 'SPHERICAL_SHELL') then
!        iflag_domain_shell = 2
!      else
        iflag_domain_shell = 1
!      end if
!      write(*,*) 'domain type', iflag_domain_shell,                    &
!     &            trim(domain_shape_ctl)
!
      if     (divide_type_ctl .eq. 'cube'                               &
     &   .or. divide_type_ctl .eq. 'Cube'                               &
     &   .or. divide_type_ctl .eq. 'CUBE') then
        iflag_mesh = 1
      else if(divide_type_ctl .eq. 'sphere'                             &
     &   .or. divide_type_ctl .eq. 'Sphere'                             &
     &   .or. divide_type_ctl .eq. 'SPHERE') then
        iflag_mesh = 2
      else
        iflag_mesh = 2
      end if
      write(*,*) 'divide_type_ctl', iflag_mesh, trim(divide_type_ctl)
!
!
      if     (high_ele_type_ctl .eq. 'quad'                             &
     &   .or. high_ele_type_ctl .eq. 'Quad'                             &
     &   .or. high_ele_type_ctl .eq. 'QUAD'                             &
     &   .or. high_ele_type_ctl .eq. 'quadrature'                       &
     &   .or. high_ele_type_ctl .eq. 'Quadrature'                       &
     &   .or. high_ele_type_ctl .eq. 'QUADRATURE') then
        iflag_quad = 1
      else if(high_ele_type_ctl .eq. 'linear'                           &
     &   .or. high_ele_type_ctl .eq. 'Linear'                           &
     &   .or. high_ele_type_ctl .eq. 'LINEAR') then
        iflag_quad = 0
      else
        iflag_quad = 1
      end if
      write(*,*) iflag_quad, trim(high_ele_type_ctl)
!
!
      num_hemi =       numele_4_90deg
      ncube_vertical = num_hemi
!
      n_shell = numlayer_shell_ctl
      nr_adj =  nend_adjust_ctl
      if(i_nstart_cube .gt. 0) then
         nr_back = nstart_cube_ctl
      else
         nr_back = n_shell
      end if
!
      if(i_numele_4_vert .gt. 0) then
        ncube_vertical = numele_4_vertical_ctl
      end if
!
      write(*,*) 'n_shell', n_shell
      write(*,*) 'nr_adj', nr_adj, nend_adjust_ctl
      write(*,*) 'nr_back', nr_back, nstart_cube_ctl
!
      call allocate_shell_radius
!
      r_nod(1:n_shell) = r_layer(1:n_shell)
!
!   set ICB and CMB address
!
      if (i_nlayer_ICB .gt. 0) then
        nlayer_ICB = nlayer_ICB_ctl
      else
        nlayer_ICB = 1
      end if
!
      if (i_nlayer_CMB .gt. 0) then
        nlayer_CMB = nlayer_CMB_ctl
      else
        nlayer_CMB = n_shell
      end if
!
      nlayer_EXT = n_shell
!
!   set node group table
!
      if_CMB = 1
      if_ICB = 1
      if_EXT = 1
      if (i_num_nod_grp .gt. 0) then
!
        do j = 1, num_node_grp_ctl
          if(nod_grp_name_ctl(j).eq.'CMB'                               &
     &     .or. nod_grp_name_ctl(j).eq.'cmb') then
            if_CMB = 0
            k = istack_nod_grp_layer_csp(j-1) + 1
            nlayer_CMB = id_nod_grp_layer_ctl(k)
          end if
          if(nod_grp_name_ctl(j).eq.'ICB'                               &
     &     .or. nod_grp_name_ctl(j).eq.'icb') then
            if_ICB = 0
            k = istack_nod_grp_layer_csp(j-1) + 1
            nlayer_ICB = id_nod_grp_layer_ctl(k)
          end if
          if(nod_grp_name_ctl(j).eq.'INFINITY'                          &
     &     .or. nod_grp_name_ctl(j).eq.'Infinity'                       &
     &     .or. nod_grp_name_ctl(j).eq.'infinity') then
            if_EXT = 0
            k = istack_nod_grp_layer_csp(j-1) + 1
            nlayer_EXT = id_nod_grp_layer_ctl(k)
          end if
        end do
        num_node_grp_csp =  num_node_grp_ctl +  if_CMB+if_ICB+if_EXT
        num_nod_layer_csp = num_nod_layer_ctl + if_CMB+if_ICB+if_EXT
      else
        num_node_grp_csp =  if_CMB+if_ICB+if_EXT
        num_nod_layer_csp = if_CMB+if_ICB+if_EXT
      end if
!
      call allocate_nod_grp_name_csp
      call allocate_nod_grp_layer_csp
!
      istack_nod_grp_layer_csp(0) = 0
      j = 0
      if(if_ICB .gt. 0) then
        j = j + 1
        istack_nod_grp_layer_csp(j) = istack_nod_grp_layer_csp(j-1) + 1
        nod_grp_name_csp(j) =        'ICB'
        id_nod_grp_layer_csp(j) = nlayer_ICB
      end if
      if(if_CMB .gt. 0) then
        j = j + 1
        istack_nod_grp_layer_csp(j) = istack_nod_grp_layer_csp(j-1) + 1
        nod_grp_name_csp(j) = 'CMB'
        id_nod_grp_layer_csp(j) = nlayer_CMB
      end if
      if(if_EXT .gt. 0) then
        j = j + 1
        istack_nod_grp_layer_csp(j) = istack_nod_grp_layer_csp(j-1) + 1
        nod_grp_name_csp(j) = 'Infinity'
        id_nod_grp_layer_csp(j) = nlayer_EXT
      end if
!
      if (i_num_nod_grp .gt. 0) then
        do j = 1, num_node_grp_ctl
          k = j + if_CMB+if_ICB+if_EXT
          istack_nod_grp_layer_csp(k) = istack_nod_grp_layer_csp(k-1)   &
     &                                 + istack_nod_grp_layer_ctl(j)    &
     &                                 - istack_nod_grp_layer_ctl(j-1)
          nod_grp_name_csp(k) = nod_grp_name_ctl(j)
        end do
        do j = 1, num_nod_layer_ctl
          k = j + istack_nod_grp_layer_csp(if_CMB+if_ICB+if_EXT)
          id_nod_grp_layer_csp(k) = id_nod_grp_layer_ctl(j)
        end do
!
        call deallocate_nod_grp_name_ctl
      end if
!
      do j = 1, num_node_grp_csp
        jst = istack_nod_grp_layer_csp(j-1) + 1
        jed = istack_nod_grp_layer_csp(j)
        write(*,*) j, istack_nod_grp_layer_csp(j),                      &
     &      trim(nod_grp_name_csp(j))
        write(*,*) id_nod_grp_layer_csp(jst:jed)
      end do
!
!   set element group table
!
      if_CMB = 1
      if_ICB = 1
      if_EXT = 1
      if (i_num_ele_grp .gt. 0) then
        do j = 1, num_ele_grp_ctl
          if(ele_grp_name_ctl(j).eq.'outer_core'                        &
     &     .or. ele_grp_name_ctl(j).eq.'OUTER_CORE') then
            if_CMB = 0
          end if
          if(ele_grp_name_ctl(j).eq.'inner_core'                        &
     &     .or. ele_grp_name_ctl(j).eq.'INNER_CORE') then
            if_ICB = 0
          end if
          if(ele_grp_name_ctl(j).eq.'external'                          &
     &     .or. ele_grp_name_ctl(j).eq.'EXTERNAL') then
            if_EXT = 0
          end if
        end do
!
        num_ele_grp_csp =   num_ele_grp_ctl + if_CMB+if_ICB+if_EXT
      else
        num_ele_grp_csp =   if_CMB+if_ICB+if_EXT
      end if
!
      call allocate_ele_grp_name_csp
!
      istack_ele_grp_layer_csp(0) = 0
      j = 0
      if(if_ICB .gt. 0) then
        j = j + 1
        istack_ele_grp_layer_csp(j) = istack_ele_grp_layer_csp(j-1)     &
     &                               + max(nlayer_ICB,0)
        ele_grp_name_csp(j) =        'inner_core'
      end if
      if(if_CMB .gt. 0) then
        j = j + 1
        istack_ele_grp_layer_csp(j) = istack_ele_grp_layer_csp(j-1)     &
     &                               + max((nlayer_CMB-nlayer_ICB),0)
        ele_grp_name_csp(j) = 'outer_core'
      end if
      if(if_EXT .gt. 0) then
        j = j + 1
        istack_ele_grp_layer_csp(j) = istack_ele_grp_layer_csp(j-1)     &
     &                               + max((nlayer_EXT-nlayer_CMB),0)
        ele_grp_name_csp(j) = 'external'
      end if
!
      num_ele_layer_csp = num_ele_layer_ctl                             &
     &                    + istack_ele_grp_layer_csp(j)
      call allocate_ele_grp_layer_csp
!
      j = 0
      if(if_ICB .gt. 0) then
        j = j + 1
        do k = 1, nlayer_ICB
          i = istack_ele_grp_layer_csp(j-1) + k
          id_ele_grp_layer_csp(i) = k - 1
        end do
      end if
      if(if_CMB .gt. 0) then
        j = j + 1
        do k = 1, (nlayer_CMB-nlayer_ICB)
          i = istack_ele_grp_layer_csp(j-1) + k
          id_ele_grp_layer_csp(i) = k + nlayer_ICB- 1
        end do
      end if
      if(if_EXT .gt. 0) then
        j = j + 1
        do k = 1, (nlayer_EXT-nlayer_CMB)
          i = istack_ele_grp_layer_csp(j-1) + k
          id_ele_grp_layer_csp(i) = k + nlayer_CMB - 1
        end do
      end if
!
!
      if (i_num_ele_grp .gt. 0) then
!
        do j = 1, num_ele_grp_ctl
          k = j + if_CMB+if_ICB+if_EXT
          istack_ele_grp_layer_csp(k) = istack_ele_grp_layer_csp(k-1)   &
     &                                 + istack_ele_grp_layer_ctl(j)    &
     &                                 - istack_ele_grp_layer_ctl(j-1)
          ele_grp_name_csp(k) = ele_grp_name_ctl(j)
        end do
        do j = 1, num_ele_layer_ctl
          k = j + istack_ele_grp_layer_csp(if_CMB+if_ICB+if_EXT)
          id_ele_grp_layer_csp(k) = id_ele_grp_layer_ctl(j)
        end do
!
        call deallocate_ele_grp_name_ctl
      end if
!
      do j = 1, num_ele_grp_csp
        jst = istack_ele_grp_layer_csp(j-1) + 1
        jed = istack_ele_grp_layer_csp(j)
        write(*,*) j, istack_ele_grp_layer_csp(j),                      &
     &      trim(ele_grp_name_csp(j))
        write(*,*) id_ele_grp_layer_csp(jst:jed)
      end do
!
!   set surface group table
!
      if_CMB = 1
      if_ICB = 1
      if_EXT = 1
      if (i_num_sf_grp .gt. 0) then
        do j = 1, num_surf_grp_ctl
          if(surf_grp_name_ctl(j).eq.'CMB_surf'                         &
     &     .or. surf_grp_name_ctl(j).eq.'CMB_SURF') then
            if_CMB = 0
          end if
          if(surf_grp_name_ctl(j).eq.'ICB_surf'                         &
     &     .or. surf_grp_name_ctl(j).eq.'ICB_SURF') then
            if_ICB = 0
          end if
          if(surf_grp_name_ctl(j).eq.'external_surf'                     &
     &     .or. surf_grp_name_ctl(j).eq.'EXTERNAL_SURF') then
            if_EXT = 0
          end if
        end do
!
        num_surf_grp_csp =   num_surf_grp_ctl + if_CMB+if_ICB+if_EXT
        num_surf_layer_csp = num_surf_layer_ctl + if_CMB+if_ICB+if_EXT
      else
        num_surf_grp_csp =   if_CMB+if_ICB+if_EXT
        num_surf_layer_csp = if_CMB+if_ICB+if_EXT
      end if
!
      call allocate_surf_grp_name_csp
      call allocate_surf_grp_layer_csp
!
      istack_surf_grp_layer_csp(0) = 0
      j = 0
      if(if_ICB .gt. 0) then
        j = j + 1
        istack_surf_grp_layer_csp(j) = istack_surf_grp_layer_csp(j-1)+1
        surf_grp_name_csp(j) =        'ICB_surf'
        id_surf_grp_layer_csp(1,j) = nlayer_ICB
        id_surf_grp_layer_csp(2,j) = 5
      end if
      if(if_CMB .gt. 0) then
        j = j + 1
        istack_surf_grp_layer_csp(j) = istack_surf_grp_layer_csp(j-1)+1
        surf_grp_name_csp(j) = 'CMB_surf'
        id_surf_grp_layer_csp(1,j) = nlayer_CMB - 1
        id_surf_grp_layer_csp(2,j) = 6
      end if
      if(if_EXT .gt. 0) then
        j = j + 1
        istack_surf_grp_layer_csp(j) = istack_surf_grp_layer_csp(j-1)+1
        surf_grp_name_csp(j) = 'Infinity_surf'
        id_surf_grp_layer_csp(1,j) = nlayer_EXT - 1
        id_surf_grp_layer_csp(2,j) = 6
      end if
!
      if (i_num_sf_grp .gt. 0) then
        do j = 1, num_surf_grp_ctl
          k = j + if_CMB+if_ICB+if_EXT
          istack_surf_grp_layer_csp(k) = istack_surf_grp_layer_csp(k-1) &
     &                                 + istack_surf_grp_layer_ctl(j)   &
     &                                 - istack_surf_grp_layer_ctl(j-1)
          surf_grp_name_csp(k) = surf_grp_name_ctl(j)
        end do
        do j = 1, num_surf_layer_ctl
          k = j + istack_surf_grp_layer_csp(if_CMB+if_ICB+if_EXT)
          id_surf_grp_layer_csp(1,k) = id_surf_grp_layer_ctl(j)
!
          if (   surf_grp_layer_type_ctl(j) .eq. 'in'                   &
     &      .or. surf_grp_layer_type_ctl(j) .eq. 'In'                   &
     &      .or. surf_grp_layer_type_ctl(j) .eq. 'IN') then
            id_surf_grp_layer_csp(2,k) = 5
          else if (surf_grp_layer_type_ctl(j) .eq. 'out'                &
     &        .or. surf_grp_layer_type_ctl(j) .eq. 'Out'                &
     &        .or. surf_grp_layer_type_ctl(j) .eq. 'OUT') then
            id_surf_grp_layer_csp(2,k) = 6
          end if
        end do
!
        if(num_surf_grp_ctl.gt.0) call deallocate_surf_grp_name_ctl
!
      end if
!
      do j = 1, num_surf_grp_csp
        jst = istack_surf_grp_layer_csp(j-1) + 1
        jed = istack_surf_grp_layer_csp(j)
        write(*,*) j, istack_surf_grp_layer_csp(j),                     &
     &      trim(surf_grp_name_csp(j))
        write(*,*) id_surf_grp_layer_csp(1,jst:jed)
        write(*,*) id_surf_grp_layer_csp(2,jst:jed)
      end do
!
!
      num_edge_latitude_ref = num_edge_latitude_ctl
      call allocate_ref_edge_latitude
!
      if(num_edge_latitude_ctl .gt. 0) then
        do j = 1, num_edge_latitude_ref
          kr_edge_latitude_ref(j) =  kr_edge_latitude_ctl(j)
          edge_latitude_ref(j) =     edge_latitude_ctl(j)
        end do
        call deallocate_edge_latitude_ctl
      end if
!
      max_coarse_level = num_level_coarse
      call allocate_coarsing_parameter
!
      icoarse_level(1:max_coarse_level,1)                               &
     &      = sp_r_coarse_ratio(1:max_coarse_level,1)
      icoarse_level(1:max_coarse_level,2)                               &
     &      = sp_r_coarse_ratio(1:max_coarse_level,2)
!
!
      write(*,*) 'num_edge_latitude_ref', num_edge_latitude_ref
      do j = 1, num_edge_latitude_ref
        write(*,*) j, kr_edge_latitude_ref(j) , edge_latitude_ref(j)
      end do
!
!
      if(num_edge_latitude_ref .gt. 0) then
        ked = kr_edge_latitude_ref(1)
        do k = 1, ked
          edge_latitude(k) = 45.0d0                                     &
     &              + (edge_latitude_ref(1) - 45.0d0)                   &
     &                               * (r_nod(k) - r_nod(1))            &
     &                               / (r_nod(ked) - r_nod(1))
        end do
!
        do j = 2, num_edge_latitude_ref
          kst = kr_edge_latitude_ref(j-1)
          ked = kr_edge_latitude_ref(j)
          do k = kst+1, ked
            edge_latitude(k) = edge_latitude_ref(j-1)                   &
     &                + (edge_latitude_ref(j) - edge_latitude_ref(j-1)) &
     &                               * (r_nod(k) - r_nod(kst))          &
     &                               / (r_nod(ked) - r_nod(kst))
          end do
        end do
!
        kst = kr_edge_latitude_ref(num_edge_latitude_ref)
        do k = kst+1, n_shell
          edge_latitude(k) = edge_latitude_ref(num_edge_latitude_ref)   &
     &            + (45.0d0 - edge_latitude_ref(num_edge_latitude_ref)) &
     &                               * (r_nod(k) - r_nod(kst))          &
     &                               / (r_nod(n_shell) - r_nod(kst))
        end do
!
        write(*,*) 'edge_latitude', n_shell
        do j = 1, n_shell
          write(*,*) j, edge_latitude(j)
        end do
      end if
!
      end subroutine set_shell_paramteres
!
!   --------------------------------------------------------------------
!
      end module set_cubed_sph_control
