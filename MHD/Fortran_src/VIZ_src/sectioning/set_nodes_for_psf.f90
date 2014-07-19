!>@file   set_nodes_for_psf.f90
!!@brief  module set_nodes_for_psf
!!
!!@date  Programmed by H.Matsui in June, 2006
!
!>@brief Check node positions to generate sections
!!
!!@verbatim
!!      subroutine count_nodes_4_psf(numedge, nnod_4_edge,              &
!!     &          ie_edge, num_surf, inod_stack_sf_grp)
!!      subroutine count_nodes_4_iso(numedge, nnod_4_edge, ie_edge)
!!
!!      subroutine set_nodes_4_psf(numnod, numedge, nnod_4_edge,        &
!!      &         inod_global, xx, ie_edge, num_surf, ntot_node_sf_grp, &
!!      &         inod_stack_sf_grp, inod_surf_grp)
!!      subroutine set_nodes_4_iso(numnod, numedge, nnod_4_edge,        &
!!     &          inod_global, xx, ie_edge)
!!@endverbatim
!
      module set_nodes_for_psf
!
      use m_precision
!
      use m_machine_parameter
      use set_node_for_sections
      use set_nodal_field_for_psf
      use set_psf_nodes_4_by_surf_grp
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_nodes_4_psf(numedge, nnod_4_edge,                &
     &          ie_edge, num_surf, inod_stack_sf_grp)
!
      use m_control_params_4_psf
      use m_patch_data_psf
      use m_psf_data
!
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
!
      integer(kind = kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
!
      integer(kind = kint) :: i, ist_smp, igrp, num
!
!
      istack_nod_psf_smp(0) = 0
      do i = 1, num_psf
        psf_list(i)%istack_n_on_n_smp(0) = 0
        psf_list(i)%istack_n_on_e_smp(0) = 0
!
!
        ist_smp = (i-1)*np_smp
!
        if( id_section_method(i) .gt. 0) then
!
!          write(*,*) 'count_node_at_node_psf'
          call count_node_at_node_psf                                   &
     &       (psf_search(i)%node_list, psf_list(i))
!
!          write(*,*) 'count_node_on_edge_4_psf'
          call count_node_on_edge_4_psf(numedge, nnod_4_edge, ie_edge,  &
     &        psf_search(i)%edge_list, psf_list(i))
!
        else if ( id_section_method(i) .eq. 0) then
!
          igrp = id_psf_group(i)
          num = inod_stack_sf_grp(igrp  ) - inod_stack_sf_grp(igrp-1)
          call count_node_at_node_on_grp(num,                           &
     &        psf_list(i)%istack_n_on_n_smp)
!
          call count_node_on_edge_on_grp(psf_list(i)%istack_n_on_e_smp)
        end if
!
!          write(*,*) 'count_position_4_psf'
        call count_position_4_psf(istack_nod_psf_smp(ist_smp),          &
     &        psf_list(i)%istack_n_on_n_smp,                            &
     &        psf_list(i)%istack_n_on_e_smp)
!
!
        psf_list(i)%nnod_on_nod = psf_list(i)%istack_n_on_n_smp(np_smp)
        psf_list(i)%nnod_on_edge= psf_list(i)%istack_n_on_e_smp(np_smp)
!
!        write(*,*) 'istack_n_on_n_smp',i,psf_list(i)%istack_n_on_n_smp
!        write(*,*) 'istack_n_on_e_smp',i,psf_list(i)%istack_n_on_e_smp
      end do
      nnod_psf_tot =         istack_nod_psf_smp(num_psf*np_smp)
!          write(*,*) 'istack_nod_psf_smp', istack_nod_psf_smp
!
      end subroutine count_nodes_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine count_nodes_4_iso(numedge, nnod_4_edge, ie_edge)
!
      use m_control_params_4_iso
      use m_patch_data_iso
      use m_iso_data
!
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
!
      integer(kind = kint) :: i, ist_smp
!
!
      istack_nod_iso_smp(0) = 0
      do i = 1, num_iso
        iso_list(i)%istack_n_on_n_smp(0) = 0
        iso_list(i)%istack_n_on_e_smp(0) = 0
!
        ist_smp = (i-1)*np_smp
        call count_node_at_node_psf                                     &
     &     (iso_search(i)%node_list, iso_list(i))
!
        call count_node_on_edge_4_psf(numedge, nnod_4_edge, ie_edge,    &
     &      iso_search(i)%edge_list, iso_list(i))
!
        call count_position_4_psf(istack_nod_iso_smp(ist_smp),          &
     &      iso_list(i)%istack_n_on_n_smp,                              &
     &      iso_list(i)%istack_n_on_e_smp)
!
        iso_list(i)%nnod_on_nod = iso_list(i)%istack_n_on_n_smp(np_smp)
        iso_list(i)%nnod_on_edge= iso_list(i)%istack_n_on_e_smp(np_smp)
      end do
      nnod_iso_tot =         istack_nod_iso_smp(num_iso*np_smp)
!
      end subroutine count_nodes_4_iso
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_nodes_4_psf(numnod, numedge, nnod_4_edge,          &
      &         inod_global, xx, ie_edge, num_surf, ntot_node_sf_grp,   &
      &         inod_stack_sf_grp, inod_surf_grp)
!
      use m_control_params_4_psf
      use m_psf_data
      use m_patch_data_psf
      use coordinate_converter
      use set_node_on_edge_quad_psf
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: num_surf, ntot_node_sf_grp
      integer(kind = kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
      integer(kind = kint), intent(in)                                  &
     &                     :: inod_surf_grp(ntot_node_sf_grp)
!
      integer(kind = kint), intent(in) :: inod_global(numnod)
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint) :: i, ist_smp, ist, num, igrp
!
      do i = 1, num_psf
        ist_smp = (i-1)*np_smp
!
        if( id_section_method(i) .gt. 0) then
!
          call set_node_at_node_psf                                     &
     &       (numnod, psf_search(i)%node_list, psf_list(i))
!
          call set_node_on_edge_4_psf(numedge, nnod_4_edge, ie_edge,    &
     &        istack_nod_psf_smp(ist_smp), psf_search(i)%edge_list,     &
     &        psf_list(i))
!
          call set_node_on_edge_4_quad_psf(numnod, numedge,             &
     &        nnod_4_edge, ie_edge, xx, const_psf(1,i),                 &
     &        psf_list(i)%nnod_on_edge, np_smp,                         &
     &        psf_list(i)%istack_n_on_e_smp, psf_list(i)%iedge_4_nod,   &
     &        psf_list(i)%coef_on_edge)
!
          call set_nod_on_nod_4_edge_psf(numedge, nnod_4_edge, ie_edge, &
     &        istack_nod_psf_smp(ist_smp), psf_search(i)%edge_list,     &
     &        psf_list(i))
!
        else if( id_section_method(i) .eq. 0) then
          igrp = id_psf_group(i)
          ist = inod_stack_sf_grp(igrp-1)
          num = inod_stack_sf_grp(igrp  ) - ist
          call set_node_at_node_on_grp                                  &
     &       (num, inod_surf_grp(ist+1), psf_list(i))
!
        end if
!
!
        call set_position_4_psf(numnod, numedge, nnod_4_edge,           &
     &      ie_edge, inod_global, xx, nnod_psf_tot,                     &
     &      istack_nod_psf_smp(ist_smp), inod_hash_psf, xyz_psf,        &
     &      psf_list(i))
      end do
!
      call position_2_sph(nnod_psf_tot, xyz_psf,                        &
     &    sph_psf(1,1), sph_psf(1,2), sph_psf(1,3),                     &
     &    sph_psf(1,4), cyl_psf(1,1), cyl_psf(1,2) )
!
      end subroutine set_nodes_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_nodes_4_iso(numnod, numedge, nnod_4_edge,          &
     &          inod_global, xx, ie_edge)
!
      use m_control_params_4_iso
      use m_iso_data
      use m_patch_data_iso
      use coordinate_converter
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: inod_global(numnod)
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint) :: i, ist_smp
!
!
      do i = 1, num_iso
!
        ist_smp = (i-1)*np_smp
        call set_node_at_node_psf                                       &
     &     (numnod, iso_search(i)%node_list, iso_list(i))
!
        call set_node_on_edge_4_psf(numedge,  nnod_4_edge, ie_edge,     &
     &      istack_nod_iso_smp(ist_smp), iso_search(i)%edge_list,       &
     &      iso_list(i))
!
        call set_nod_on_nod_4_edge_psf(numedge, nnod_4_edge, ie_edge,   &
     &      istack_nod_iso_smp(ist_smp), iso_search(i)%edge_list,       &
     &      iso_list(i))
!
!
        call set_position_4_psf(numnod, numedge, nnod_4_edge,           &
     &      ie_edge, inod_global, xx, nnod_iso_tot,                     &
     &      istack_nod_iso_smp(ist_smp), inod_hash_iso, xyz_iso,        &
     &      iso_list(i))
      end do
!
!
      call position_2_sph(nnod_iso_tot, xyz_iso,                        &
     &    sph_iso(1,1), sph_iso(1,2), sph_iso(1,3),                     &
     &    sph_iso(1,4), cyl_iso(1,1), cyl_iso(1,2) )
!
      end subroutine set_nodes_4_iso
!
!  ---------------------------------------------------------------------
!
      end module set_nodes_for_psf
