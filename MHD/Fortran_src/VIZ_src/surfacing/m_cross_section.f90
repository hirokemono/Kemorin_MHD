!>@file   m_cross_section.f90
!!@brief  module m_cross_section
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Structure for cross sectioning
!!
!!@verbatim
!!      subroutine cross_section_init                                   &
!!     &         (numnod, internal_node, numele, numsurf, numedge,      &
!!     &          nnod_4_ele, nnod_4_edge, ie, ie_edge, isf_4_ele,      &
!!     &          iedge_4_sf, iedge_4_ele, nod_comm, edge_comm,         &
!!     &          iedge_4_ele, nod_comm, edge_comm,                     &
!!     &          interior_ele, xx, inod_smp_stack, iele_smp_stack,     &
!!     &          isurf_smp_stack, iedge_smp_stack,                     &
!!     &          num_mat, num_mat_bc, mat_name, mat_istack, mat_item,  &
!!     &          num_surf, num_surf_bc, surf_name, surf_istack,        &
!!     &          surf_item, ntot_node_sf_grp, inod_stack_sf_grp,       &
!!     &          inod_surf_grp, num_nod_phys, phys_nod_name)
!!
!!      subroutine cross_section_main(istep_psf, numnod, numedge,       &
!!     &          nnod_4_edge, ie_edge, num_nod_phys, num_tot_nod_phys, &
!!     &          istack_nod_component, d_nod)
!!
!!      subroutine dealloc_psf_field_type
!!@endverbatim
!
!
      module m_cross_section
!
      use calypso_mpi
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_phys_data
      use t_psf_geometry_list
      use t_psf_patch_data
      use t_ucd_data
!
      implicit  none
!
!>      Number of sections
      integer(kind = kint) :: num_psf
!
!>      Structure for table for sections
      type(sectioning_list), allocatable, save :: psf_list(:)
!
!>      Structure for search table for sections
      type(psf_search_lists), allocatable, save :: psf_search(:)
!
      type(psf_parameters), allocatable, save :: psf_param(:)
!
!>      Structure for psf patch data on local domain
      type(psf_local_data), allocatable, save :: psf_mesh(:)
!
!>      Structure for cross sectioning output (used by master process)
      type(ucd_data), allocatable, save :: psf_out(:)
      type(merged_ucd_data), allocatable, save :: psf_out_m(:)
!
      private :: alloc_psf_field_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cross_section_init                                     &
     &         (numnod, internal_node, numele, numsurf, numedge,        &
     &          nnod_4_ele, nnod_4_edge, ie, ie_edge, isf_4_ele,        &
     &          iedge_4_sf, iedge_4_ele, nod_comm, edge_comm,           &
     &          interior_ele, xx, inod_smp_stack, iele_smp_stack,       &
     &          isurf_smp_stack, iedge_smp_stack,                       &
     &          num_mat, num_mat_bc, mat_name, mat_istack, mat_item,    &
     &          num_surf, num_surf_bc, surf_name, surf_istack,          &
     &          surf_item, ntot_node_sf_grp, inod_stack_sf_grp,         &
     &          inod_surf_grp, num_nod_phys, phys_nod_name)
!
!
      use m_geometry_constants
      use m_control_params_4_psf
!
      use calypso_mpi
      use t_comm_table
      use set_psf_iso_control
      use search_ele_list_for_psf
      use set_const_4_sections
      use find_node_and_patch_psf
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
!
      integer(kind=kint), intent(in) :: numnod, internal_node, numele
      integer(kind=kint), intent(in) :: numsurf, numedge
      integer(kind=kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind=kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind=kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind=kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in)                                  &
     &              :: iedge_4_sf(numsurf,nedge_4_surf)
      integer(kind=kint), intent(in) :: iedge_4_ele(numele,nedge_4_ele)
      integer(kind=kint), intent(in) :: interior_ele(numele)
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
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: edge_comm
!
      integer(kind = kint) :: i_psf
!
!
      call alloc_psf_field_type
!
      call set_psf_control                                              &
     &   (num_psf, num_mat, mat_name, num_surf, surf_name,              &
     &    num_nod_phys, phys_nod_name, psf_param, psf_mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'set_search_mesh_list_4_psf'
      call set_search_mesh_list_4_psf(num_psf,                          &
     &        numnod, numele, numsurf, numedge, nnod_4_edge, ie_edge,   &
     &        isf_4_ele, iedge_4_sf, interior_ele, inod_smp_stack,      &
     &        iele_smp_stack, isurf_smp_stack, iedge_smp_stack,         &
     &        num_mat, num_mat_bc, mat_istack, mat_item,                &
     &        psf_param, psf_search)
!
!
      do i_psf = 1, num_psf
        if (iflag_debug.eq.1) write(*,*) 'alloc_numnod_stack', i_psf
        call allocate_node_param_smp_type(psf_mesh(i_psf)%node)
        call allocate_ele_param_smp_type(psf_mesh(i_psf)%patch)
!
        if (iflag_debug.eq.1) write(*,*) 'alloc_ref_field_4_psf'
        call alloc_ref_field_4_psf(numnod, psf_list(i_psf))
        if (iflag_debug.eq.1) write(*,*) 'alloc_nnod_psf'
        call alloc_nnod_psf(np_smp, numnod, numedge, psf_list(i_psf))
      end do
!
      if (iflag_debug.eq.1) write(*,*) 'set_const_4_crossections'
      call set_const_4_crossections                                     &
     &   (num_psf, numnod, inod_smp_stack, xx, psf_list)
!
      if (iflag_debug.eq.1) write(*,*) 'set_node_and_patch_psf'
      call set_node_and_patch_psf                                       &
     &   (num_psf, numnod, internal_node, numele, numedge, nnod_4_ele,  &
     &    nnod_4_edge, xx, ie, ie_edge, iedge_4_ele, nod_comm,          &
     &    edge_comm, num_surf, num_surf_bc, surf_istack, surf_item,     &
     &    ntot_node_sf_grp, inod_stack_sf_grp, inod_surf_grp,           &
     &    psf_search, psf_list, psf_mesh)
!
      do i_psf = 1, num_psf
        call alloc_dat_on_patch_psf(psf_mesh(i_psf))
        call alloc_phys_data_type                                       &
     &     (psf_mesh(i_psf)%node%numnod, psf_mesh(i_psf)%field)
      end do
!
      do i_psf = 1, num_psf
        psf_out(i_psf)%file_prefix = psf_header(i_psf)
        psf_out(i_psf)%ifmt_file = itype_psf_file(i_psf)
!
        call link_node_data_type_2_output                               &
     &     (psf_mesh(i_psf)%node, psf_out(i_psf))
        call link_ele_data_type_2_output                                &
     &     (psf_mesh(i_psf)%patch, psf_out(i_psf))
        call link_field_data_type_2_output(psf_mesh(i_psf)%node%numnod, &
     &      psf_mesh(i_psf)%field, psf_out(i_psf))
!
        call alloc_merged_ucd_stack(nprocs, psf_out_m(i_psf))
        psf_out_m(i_psf)%istack_merged_nod                              &
     &           => psf_mesh(i_psf)%node%istack_numnod
        psf_out_m(i_psf)%istack_merged_intnod                           &
     &           => psf_mesh(i_psf)%node%istack_internod
        psf_out_m(i_psf)%istack_merged_ele                              &
     &           => psf_mesh(i_psf)%patch%istack_numele
!
        call sel_write_parallel_ucd_mesh                                &
     &     (psf_out(i_psf), psf_out_m(i_psf))
      end do
!
!      call calypso_mpi_barrier
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
      use set_fields_for_psf
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
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
      integer(kind = kint) :: i_psf
!
!
!      call start_eleps_time(20)
      call set_field_4_psf(num_psf, numnod, numedge, nnod_4_edge,       &
     &    ie_edge, num_nod_phys, num_tot_nod_phys,                      &
     &    istack_nod_component, d_nod, psf_param, psf_list, psf_mesh)
!      call end_eleps_time(20)
!
!      call start_eleps_time(21)
      do i_psf = 1, num_psf
         call sel_write_parallel_ucd_file                               &
     &      (istep_psf, psf_out(i_psf), psf_out_m(i_psf))
      end do
!
!        call sel_write_parallel_ucd_mesh(ucd, m_ucd)
!
!        call disconnect_ucd_data(ucd)
!      end do
!      call end_eleps_time(21)
!
      end subroutine cross_section_main
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_psf_field_type
!
!
      allocate(psf_mesh(num_psf))
      allocate(psf_list(num_psf))
      allocate(psf_search(num_psf))
      allocate(psf_param(num_psf))
!
      allocate( psf_out(num_psf) )
      allocate( psf_out_m(num_psf) )
!
      end subroutine alloc_psf_field_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_psf_field_type
!
!
      deallocate(psf_mesh, psf_list)
      deallocate(psf_search, psf_out, psf_out_m, psf_param)
!
      end subroutine dealloc_psf_field_type
!
!  ---------------------------------------------------------------------
!
      end module m_cross_section
