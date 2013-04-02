!set_grouping_geometry.f90
!      module set_grouping_geometry
!
      module set_grouping_geometry
!
!      Written by Kemorin on Oct., 2007
!
!
      use m_precision
!
      use m_constants
      use m_add_ele_grp_parameter
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
!
      implicit    none
!
!      subroutine set_grid_4_sph_layering
!      subroutine set_grid_4_rs_layering
!      subroutine set_surf_4_sph_layering(nri, lth)
!      subroutine set_edge_4_sph_layering(nri, lth)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_num_grid_4_sph_layering(num_grp_1, num_grp_2)
!
      integer(kind = kint), intent(in) :: num_grp_1, num_grp_2
      integer(kind = kint) :: inod
!
!
        numnod = (num_grp_1+1) * (num_grp_2+1)
        numele = num_grp_1 * num_grp_2
        numedge = num_grp_1 * (num_grp_2+1)  + (num_grp_1+1) * num_grp_2
!
      internal_node = numnod
      numsurf = numele
      nnod_4_ele =  4
      nnod_4_edge = 2
!
      call allocate_node_geometry
      call allocate_element_connection
      call allocate_edge_connect
      call allocate_edge_4_ele
!
      do inod = 1, numnod
        globalnodid(inod) = inod
        longitude(inod) = zero
      end do
!
      end subroutine set_num_grid_4_sph_layering
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_grid_4_sph_layering
!
      integer(kind = kint) :: inod, kr, lt
!
!
      do kr = 1, num_r_ele_grp
        do lt = 1, num_t_ele_grp
          inod = lt + (kr-1) * (num_t_ele_grp+1)
          radius(inod) = minmax_r_ele_grping(kr,1)
          colatitude(inod) = minmax_t_ele_grping(lt,1)
        end do
        inod = kr * (num_t_ele_grp+1)
        radius(inod) = minmax_r_ele_grping(kr,1)
        colatitude(inod) =  minmax_t_ele_grping(num_t_ele_grp,2)
      end do
      do lt = 1, num_t_ele_grp
        inod = lt + num_r_ele_grp * (num_t_ele_grp+1)
        radius(inod) = minmax_r_ele_grping(num_r_ele_grp,2)
        colatitude(inod) =  minmax_t_ele_grping(lt,1)
      end do
      inod = (num_r_ele_grp+1) * (num_t_ele_grp+1)
      radius(inod) =     minmax_r_ele_grping(num_r_ele_grp,2)
      colatitude(inod) = minmax_t_ele_grping(num_t_ele_grp,2)
!
      end subroutine set_grid_4_sph_layering
!
!  ---------------------------------------------------------------------
!
      subroutine set_grid_4_rs_layering
!
      integer(kind = kint) :: inod, kr, ks
!
!
      do kr = 1, num_r_ele_grp
        do ks = 1, num_s_ele_grp
          inod = ks + (kr-1) * (num_s_ele_grp+1)
          radius(inod) =     minmax_r_ele_grping(kr,1)
          s_cylinder(inod) = minmax_s_ele_grping(ks,1)
          xx(inod,1) =       minmax_s_ele_grping(ks,1)
          xx(inod,2) =       minmax_s_ele_grping(ks,1)
        end do
        inod = kr * (num_s_ele_grp+1)
        radius(inod) =     minmax_r_ele_grping(kr,1)
        s_cylinder(inod) = minmax_s_ele_grping(num_s_ele_grp,2)
        xx(inod,1) =       minmax_s_ele_grping(num_s_ele_grp,2)
      end do
      do ks = 1, num_s_ele_grp
        inod = ks + num_r_ele_grp * (num_s_ele_grp+1)
        radius(inod) =     minmax_r_ele_grping(num_r_ele_grp,2)
        s_cylinder(inod) = minmax_s_ele_grping(ks,1)
        xx(inod,1) =       minmax_s_ele_grping(ks,1)
      end do
      inod = (num_r_ele_grp+1) * (num_s_ele_grp+1)
      radius(inod) =     minmax_r_ele_grping(num_r_ele_grp,2)
      s_cylinder(inod) = minmax_s_ele_grping(num_s_ele_grp,2)
      xx(inod,1) =       minmax_s_ele_grping(num_s_ele_grp,2)
!
      end subroutine set_grid_4_rs_layering
!
!  ---------------------------------------------------------------------
!
      subroutine set_grid_4_sz_layering
!
      integer(kind = kint) :: inod, ks, iz
!
!
      do ks = 1, num_s_ele_grp
        do iz = 1, num_z_ele_grp
          inod = iz + (ks-1) * (num_z_ele_grp+1)
          s_cylinder(inod) = minmax_s_ele_grping(ks,1)
          xx(inod,3) = minmax_t_ele_grping(iz,1)
        end do
        inod = ks * (num_z_ele_grp+1)
        s_cylinder(inod) = minmax_s_ele_grping(ks,1)
        xx(inod,3) =  minmax_t_ele_grping(num_z_ele_grp,2)
      end do
      do iz = 1, num_z_ele_grp
        inod = iz + num_s_ele_grp * (num_z_ele_grp+1)
        s_cylinder(inod) = minmax_s_ele_grping(num_s_ele_grp,2)
        xx(inod,3) =  minmax_t_ele_grping(iz,1)
      end do
      inod = (num_s_ele_grp+1) * (num_z_ele_grp+1)
      s_cylinder(inod) =     minmax_s_ele_grping(num_s_ele_grp,2)
      xx(inod,3) = minmax_t_ele_grping(num_z_ele_grp,2)
!
      end subroutine set_grid_4_sz_layering
!
!  ---------------------------------------------------------------------
!
      subroutine set_grid_4_zt_layering
!
      integer(kind = kint) :: inod, iz, lt
!
!
      do iz = 1, num_z_ele_grp
        do lt = 1, num_t_ele_grp
          inod = lt + (iz-1) * (num_t_ele_grp+1)
          xx(inod,3) =       minmax_z_ele_grping(iz,1)
          colatitude(inod) = minmax_t_ele_grping(lt,1)
        end do
        inod = iz * (num_t_ele_grp+1)
        xx(inod,3) =       minmax_z_ele_grping(iz,1)
        colatitude(inod) = minmax_t_ele_grping(num_t_ele_grp,2)
      end do
      do lt = 1, num_t_ele_grp
        inod = lt + num_z_ele_grp * (num_t_ele_grp+1)
        xx(inod,3) =       minmax_z_ele_grping(num_z_ele_grp,2)
        colatitude(inod) = minmax_t_ele_grping(lt,1)
      end do
      inod = (num_z_ele_grp+1) * (num_t_ele_grp+1)
      xx(inod,3) =       minmax_z_ele_grping(num_z_ele_grp,2)
      colatitude(inod) = minmax_t_ele_grping(num_t_ele_grp,2)
!
      do iz = 1, num_z_ele_grp+1
        do lt = 1, num_t_ele_grp+1
          inod = lt + (iz-1) * (num_t_ele_grp+1)
          s_cylinder(inod) = xx(inod,3) / tan(colatitude(inod))
        end do
      end do
!
      end subroutine set_grid_4_zt_layering
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_surf_4_sph_layering(nri, lth)
!
      integer(kind = kint), intent(in) :: nri, lth
      integer(kind = kint) :: iele, kr, lt
!
!
      do kr = 1, nri
        do lt = 1, lth
          iele = lt +   (kr-1) * lth
          globalelmid(iele) = iele
          elmtyp(iele) = 221
          nodelm(iele) = 4
          ie(iele,1) = lt +   (kr-1) * (lth+1)
          ie(iele,2) = lt+1 + (kr-1) * (lth+1)
          ie(iele,3) = lt+1 + kr *     (lth+1)
          ie(iele,4) = lt +   kr *     (lth+1)
        end do
      end do
!
      end subroutine set_surf_4_sph_layering
!
!  ---------------------------------------------------------------------
!
      subroutine set_edge_4_sph_layering(nri, lth)
!
      integer(kind = kint), intent(in) :: nri, lth
      integer(kind = kint) :: iele, iedge, kr, lt
!
!
      do kr = 1, nri + 1
        do lt = 1, lth
          iedge = lt +   (kr-1) * lth
          globaledgeid(iedge) = iedge
          ie_edge(iedge,1) = lt +   (kr-1) * lth
          ie_edge(iedge,2) = lt+1 + (kr-1) * lth
        end do
      end do
!
      do kr = 1, nri
        do lt = 1, lth+1
          iedge = lt + (kr-1) * (lth+1) + (nri+1)*lth
          globaledgeid(iedge) = iedge
          ie_edge(iedge,1) = lt + (kr-1) * (lth+1)
          ie_edge(iedge,2) = lt + kr *     (lth+1)
        end do
      end do
!
      do kr = 1, nri
        do lt = 1, lth
          iele = lt +   (kr-1) * lth
          iedge_4_sf(iele,1) = lt +   (kr-1) * lth
          iedge_4_sf(iele,2) = lt +   (kr-1) * (lth+1) + (nri+1)*lth
          iedge_4_sf(iele,3) = lt +    kr *    lth
          iedge_4_sf(iele,4) = lt+1 + (kr-1) * (lth+1) + (nri+1)*lth
        end do
      end do
!
      end subroutine set_edge_4_sph_layering
!
!  ---------------------------------------------------------------------
!
      end module set_grouping_geometry
