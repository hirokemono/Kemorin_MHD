!set_patches_for_psf.f90
!      module set_patches_for_psf
!
!      Written by H. Matsui on June, 2006
!
!      subroutine count_psf_patches(numnod, numele, numedge, nnod_4_ele,&
!     &          ie, iedge_4_ele, num_surf_grp, istack_surf_grp)
!      subroutine count_iso_patches(numnod, numele, numedge, nnod_4_ele,&
!     &          ie, iedge_4_ele)
!
!      subroutine set_psf_patches(numele, numedge, nnod_4_ele, ie,      &
!     &          iedge_4_ele, num_surf_grp, ntot_surf_grp,              &
!     &          istack_surf_grp, item_surf_grp)
!      subroutine set_iso_patches(numele, numedge, iedge_4_ele)
!
      module set_patches_for_psf
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_psf_patches(numnod, numele, numedge, nnod_4_ele, &
     &          ie, iedge_4_ele, num_surf_grp, istack_surf_grp)
!
      use m_geometry_constants
      use m_control_params_4_psf
      use m_search_list_4_psf
      use m_geometry_list_4_psf
      use m_patch_data_psf
!
      use set_psf_patch_4_by_surf_grp
      use patch_4_psf
!
      integer(kind = kint), intent(in) :: numnod, numele, numedge
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in)                                  &
     &                     :: iedge_4_ele(numele,nedge_4_ele)
!
      integer(kind = kint), intent(in) :: num_surf_grp
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_surf_grp(0:num_surf_grp)
!
      integer(kind = kint) :: i, ist_smp
!
!
      istack_patch_psf_smp(0) = 0
      do i = 1, num_psf
!
        ist_smp = (i-1)*np_smp
        if( id_section_method(i) .gt. 0) then
!
          call set_psf_type_id(numnod, numele, nnod_4_ele, ie,          &
     &        nele_search_psf_tot, istack_ele_search_psf_s(ist_smp),    &
     &        iele_search_psf, c_ref_psf(1,i), mark_ele_psf )
!
          call count_num_patch_4_psf(numele, numedge, iedge_4_ele,      &
     &        nele_search_psf_tot, istack_ele_search_psf_s(ist_smp),    &
     &        iele_search_psf, id_n_on_e_psf(1,i), mark_ele_psf,        &
     &        istack_patch_psf_smp(ist_smp) )
!
        else if( id_section_method(i) .eq. 0) then
          call count_num_patch_4_grp(num_surf_grp, istack_surf_grp,     &
     &        id_psf_group(i),  istack_patch_psf_smp(ist_smp) )
!
        end if
!
      end do
      npatch_tot_psf_smp = istack_patch_psf_smp(num_psf*np_smp)
!
      end subroutine count_psf_patches
!
!  ---------------------------------------------------------------------
!
      subroutine count_iso_patches(numnod, numele, numedge, nnod_4_ele, &
     &          ie, iedge_4_ele)
!
      use m_geometry_constants
      use m_control_params_4_iso
      use m_geometry_list_4_iso
      use m_search_list_4_iso
      use m_patch_data_iso
!
      use patch_4_psf
!
      integer(kind = kint), intent(in) :: numnod, numele, numedge
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: iedge_4_ele(numele,nedge_4_ele)
!
      integer(kind = kint) :: i, ist_smp
!
!
      istack_patch_iso_smp(0) = 0
      do i = 1, num_iso
!
        ist_smp = (i-1)*np_smp
        call set_psf_type_id(numnod, numele, nnod_4_ele, ie,            &
     &      nele_search_iso_tot, istack_ele_search_iso_s(ist_smp),      &
     &      iele_search_iso, c_ref_iso(1,i), mark_ele_iso )
!
        call count_num_patch_4_psf(numele, numedge, iedge_4_ele,        &
     &      nele_search_iso_tot, istack_ele_search_iso_s(ist_smp),      &
     &      iele_search_iso, id_n_on_e_iso(1,i), mark_ele_iso,          &
     &      istack_patch_iso_smp(ist_smp) )
      end do
      npatch_tot_iso_smp = istack_patch_iso_smp(num_iso*np_smp)
!
      end subroutine count_iso_patches
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_psf_patches(numele, numedge, nnod_4_ele, ie,       &
     &          iedge_4_ele, num_surf_grp, ntot_surf_grp,               &
     &          istack_surf_grp, item_surf_grp)
!
      use m_geometry_constants
      use m_control_params_4_psf
      use m_search_list_4_psf
      use m_geometry_list_4_psf
      use m_patch_data_psf
!
      use set_psf_patch_4_by_surf_grp
      use patch_4_psf
!
      integer(kind = kint), intent(in) :: numele, numedge
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: iedge_4_ele(numele,nedge_4_ele)
!
      integer(kind = kint), intent(in) :: num_surf_grp, ntot_surf_grp
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_surf_grp(0:num_surf_grp)
      integer(kind = kint), intent(in)                                  &
     &                      :: item_surf_grp(2,ntot_surf_grp)
!
      integer(kind = kint) :: i, ist_smp
!
      do i = 1, num_psf
!
        ist_smp = (i-1)*np_smp
        if( id_section_method(i) .gt. 0) then
!
          call set_patch_4_psf(numele, numedge, iedge_4_ele,            &
     &        nele_search_psf_tot, istack_ele_search_psf_s(ist_smp),    &
     &        iele_search_psf, id_n_on_e_psf(1,i), mark_ele_psf,        &
     &        npatch_tot_psf_smp, istack_patch_psf_smp(ist_smp),        &
     &        ie_patch_psf)
!
        else if( id_section_method(i) .eq. 0) then
          call set_patch_4_grp(numele, numele, nnod_4_ele, ie,          &
     &        num_surf_grp, ntot_surf_grp, istack_surf_grp,             &
     &        item_surf_grp, id_psf_group(i), id_n_on_n_psf(1,i),       &
     &        npatch_tot_psf_smp,  istack_patch_psf_smp(ist_smp),       &
     &        ie_patch_psf)
!
        end if
!
        call renumber_patch_id_psf(npatch_tot_psf_smp,                  &
     &      istack_nod_psf_smp(ist_smp), istack_patch_psf_smp(ist_smp), &
     &      ie_patch_psf)
!
      end do
!
      end subroutine set_psf_patches
!
!  ---------------------------------------------------------------------
!
      subroutine set_iso_patches(numele, numedge, iedge_4_ele)
!
      use m_geometry_constants
      use m_control_params_4_iso
      use m_search_list_4_iso
      use m_geometry_list_4_iso
      use m_patch_data_iso
!
      use patch_4_psf
!
      integer(kind = kint), intent(in) :: numele, numedge
      integer(kind = kint), intent(in) :: iedge_4_ele(numele,nedge_4_ele)
!
      integer(kind = kint) :: i, ist_smp
!
      do i = 1, num_iso
!
        ist_smp = (i-1)*np_smp
        call set_patch_4_psf(numele, numedge, iedge_4_ele,              &
     &      nele_search_iso_tot, istack_ele_search_iso_s(ist_smp),      &
     &      iele_search_iso, id_n_on_e_iso(1,i), mark_ele_iso,          &
     &      npatch_tot_iso_smp, istack_patch_iso_smp(ist_smp),          &
     &      ie_patch_iso)
!
        call renumber_patch_id_psf(npatch_tot_iso_smp,                  &
     &      istack_nod_iso_smp(ist_smp), istack_patch_iso_smp(ist_smp), &
     &      ie_patch_iso)
!
      end do
!      write(*,*) 'ie_patch_iso', ie_patch_iso(:,1)
!
      end subroutine set_iso_patches
!
!  ---------------------------------------------------------------------
!
      end module set_patches_for_psf
