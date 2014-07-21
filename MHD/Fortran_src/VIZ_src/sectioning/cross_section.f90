!
!      module cross_section
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine cross_section_init(numnod, numele, numsurf, numedge, &
!!     &          nnod_4_ele, nnod_4_edge, ie, ie_edge,                 &
!!     &          isf_4_ele, iedge_4_sf, iedge_4_ele,                   &
!!     &          interior_ele, globalnodid, xx,                        &
!!     &          inod_smp_stack, iele_smp_stack,                       &
!!     &          isurf_smp_stack, iedge_smp_stack,                     &
!!     &          num_mat, num_mat_bc, mat_name, mat_istack, mat_item,  &
!!     &          num_surf, num_surf_bc, surf_name, surf_istack,        &
!!     &          surf_item, ntot_node_sf_grp, inod_stack_sf_grp,       &
!!     &          inod_surf_grp, num_nod_phys, phys_nod_name)
!!
!!      subroutine cross_section_main(istep_psf, numnod, numedge,       &
!!     &          nnod_4_edge, ie_edge, num_nod_phys, num_tot_nod_phys, &
!!     &          istack_nod_component, d_nod)
!
!
      module cross_section
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cross_section_init(numnod, numele, numsurf, numedge,   &
     &          nnod_4_ele, nnod_4_edge, ie, ie_edge,                   &
     &          isf_4_ele, iedge_4_sf, iedge_4_ele,                     &
     &          interior_ele, globalnodid, xx,                          &
     &          inod_smp_stack, iele_smp_stack,                         &
     &          isurf_smp_stack, iedge_smp_stack,                       &
     &          num_mat, num_mat_bc, mat_name, mat_istack, mat_item,    &
     &          num_surf, num_surf_bc, surf_name, surf_istack,          &
     &          surf_item, ntot_node_sf_grp, inod_stack_sf_grp,         &
     &          inod_surf_grp, num_nod_phys, phys_nod_name)
!
!
      use m_geometry_constants
      use m_psf_data
!
      use set_psf_iso_control
      use search_ele_list_for_psf
      use set_const_4_sections
      use find_node_and_patch_psf
      use collect_psf_data
      use output_section_files
!
      integer(kind=kint), intent(in) :: numnod, numele
      integer(kind=kint), intent(in) :: numsurf, numedge
      integer(kind=kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind=kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind=kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind=kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in)                                  &
     &              :: iedge_4_sf(numsurf,nedge_4_surf)
      integer(kind=kint), intent(in) :: iedge_4_ele(numele,nedge_4_ele)
      integer(kind=kint), intent(in) :: interior_ele(numele)
      integer(kind=kint), intent(in) :: globalnodid(numnod)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: isurf_smp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: iedge_smp_stack(0:np_smp)
!
      integer(kind=kint), intent(in) :: num_mat, num_mat_bc
      integer(kind=kint), intent(in) :: mat_istack(0:num_mat)
      integer(kind=kint), intent(in) :: mat_item(num_mat_bc)
      character(len=kchara), intent(in) :: mat_name(num_mat)
      integer(kind=kint), intent(in) :: num_surf, num_surf_bc
      integer(kind=kint), intent(in) :: surf_istack(0:num_surf)
      integer(kind=kint), intent(in) :: surf_item(2,num_surf_bc)
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      integer(kind=kint), intent(in) :: ntot_node_sf_grp
      integer(kind=kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
      integer(kind=kint), intent(in) :: inod_surf_grp(ntot_node_sf_grp)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      integer(kind = kint) :: i_psf
!
!
      call set_psf_control(num_mat, mat_name, num_surf, surf_name,      &
     &    num_nod_phys, phys_nod_name)
!
      if (iflag_debug.eq.1) write(*,*) 'set_search_mesh_list_4_psf'
      call set_search_mesh_list_4_psf                                   &
     &       (numnod, numele, numsurf, numedge, nnod_4_edge, ie_edge,   &
     &        isf_4_ele, iedge_4_sf, interior_ele, inod_smp_stack,      &
     &        iele_smp_stack, isurf_smp_stack, iedge_smp_stack,         &
     &        num_mat, num_mat_bc, mat_istack, mat_item)
!
!
      do i_psf = 1, num_psf
        call alloc_ref_field_4_psf(numnod, psf_list(i_psf))
        call alloc_nnod_psf(np_smp, numnod, numedge, psf_list(i_psf))
      end do
      call allocate_num_patch_psf(np_smp, num_psf)
!
      if (iflag_debug.eq.1) write(*,*) 'set_const_4_crossections'
      call set_const_4_crossections(numnod, inod_smp_stack, xx)
!
      if (iflag_debug.eq.1) write(*,*) 'set_node_and_patch_psf'
      call set_node_and_patch_psf(numnod, numele, numedge, nnod_4_ele,  &
     &    nnod_4_edge, globalnodid, xx, ie, ie_edge, iedge_4_ele,       &
     &    num_surf, num_surf_bc, surf_istack, surf_item,                &
     &    ntot_node_sf_grp, inod_stack_sf_grp, inod_surf_grp,           &
     &    istack_nod_psf_smp, istack_patch_psf_smp, psf_search,         &
     &    psf_list, psf_pat)
!
      psf_pat%max_ncomp_psf = max_ncomp_psf_out
      call alloc_dat_on_patch_psf(psf_pat)
      call alloc_psf_outputs_num(nprocs, num_psf, psf_col)
!
      if (iflag_debug.eq.1) write(*,*) 'collect_numbers_4_psf'
      call collect_numbers_4_psf(num_psf, psf_header, itype_psf_file,   &
     &    istack_nod_psf_smp, istack_patch_psf_smp,                     &
     &    psf_fld, psf_col, psf_out)
!
      call alloc_psf_outputs_data(psf_col)
      call alloc_SR_array_psf(my_rank, psf_pat%max_ncomp_psf,           &
     &    psf_pat%nnod_psf_tot, psf_pat%npatch_tot, psf_col)
!
      if (iflag_debug.eq.1) write(*,*) 'collect_mesh_4_psf'
      call collect_mesh_4_psf(num_psf, psf_pat, psf_col, psf_out)
!
      if (iflag_debug.eq.1) write(*,*) 'output_psf_grids'
      call output_psf_grids
!
      end subroutine cross_section_init
!
!  ---------------------------------------------------------------------
!
      subroutine cross_section_main(istep_psf, numnod, numedge,         &
     &          nnod_4_edge, ie_edge, num_nod_phys, num_tot_nod_phys,   &
     &          istack_nod_component, d_nod)
!
      use m_control_params_4_psf
      use m_psf_data
      use set_fields_for_psf
      use collect_psf_data
      use output_section_files
!
      integer(kind = kint), intent(in) :: istep_psf
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind=kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      integer(kind = kint), intent(in) :: num_tot_nod_phys
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_nod_component(0:num_nod_phys)
      real(kind = kreal), intent(in)  :: d_nod(numnod,num_tot_nod_phys)
!
!
!      call start_eleps_time(20)
      if (iflag_debug.eq.1) write(*,*) 'set_field_4_psf'
      call set_field_4_psf(num_psf, numnod, numedge, nnod_4_edge,       &
     &    ie_edge, istack_nod_psf_smp, num_nod_phys, num_tot_nod_phys,  &
     &    istack_nod_component, d_nod, psf_param, psf_fld, psf_list,    &
     &    psf_pat)
!      call end_eleps_time(20)
!
!      call start_eleps_time(21)
      if (iflag_debug.eq.1) write(*,*) 'collect_field_4_psf'
      call collect_field_4_psf(num_psf, psf_pat, psf_col, psf_out)
!      call end_eleps_time(21)
!
!      call start_eleps_time(22)
      if (iflag_debug.eq.1) write(*,*) 'output_psf_fields'
      call output_psf_fields(istep_psf)
!      call end_eleps_time(22)
!
      end subroutine cross_section_main
!
!  ---------------------------------------------------------------------
!
      end module cross_section
