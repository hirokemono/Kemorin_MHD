!
!      module isosurface
!
!      Written by H. Matsui on July, 2006
!
!      subroutine isosurface_init                                       &
!     &         (numnod, numele, numsurf, numedge, nnod_4_edge,         &
!     &          ie_edge, isf_4_ele, iedge_4_sf, interior_ele,          &
!     &          inod_smp_stack, iele_smp_stack,                        &
!     &          isurf_smp_stack, iedge_smp_stack,                      &
!     &          num_mat, num_mat_bc, mat_name, mat_istack, mat_item,   &
!     &          num_nod_phys, phys_nod_name)
!
!      subroutine isosurface_main(istep_iso,                            &
!     &          numnod, numele, numedge, nnod_4_ele, nnod_4_edge,      &
!     &          ie, ie_edge, iedge_4_ele, globalnodid,                 &
!     &          xx, radius, a_radius, s_cylinder, a_s_cylinder,        &
!     &          inod_smp_stack, num_nod_phys, num_tot_nod_phys,        &
!     &          istack_nod_component, d_nod)
!
      module isosurface
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_parallel_var_dof
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine isosurface_init                                        &
     &         (numnod, numele, numsurf, numedge, nnod_4_edge,          &
     &          ie_edge, isf_4_ele, iedge_4_sf, interior_ele,           &
     &          inod_smp_stack, iele_smp_stack,                         &
     &          isurf_smp_stack, iedge_smp_stack,                       &
     &          num_mat, num_mat_bc, mat_name, mat_istack, mat_item,    &
     &          num_nod_phys, phys_nod_name)
!
      use m_geometry_constants
      use m_control_params_4_iso
      use m_geometry_list_4_iso
      use m_patch_data_iso
      use m_iso_outputs
!
      use set_psf_iso_control
      use search_ele_list_for_iso
!
      integer(kind=kint), intent(in) :: numnod, numele
      integer(kind=kint), intent(in) :: numsurf, numedge
      integer(kind=kint), intent(in) :: nnod_4_edge
      integer(kind=kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind=kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind=kint), intent(in) :: iedge_4_sf(numsurf,nedge_4_surf)
      integer(kind=kint), intent(in) :: interior_ele(numele)
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
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_iso_control'
      call set_iso_control(num_mat, mat_name,                           &
     &    num_nod_phys, phys_nod_name)
!
      if (iflag_debug.eq.1) write(*,*) 'set_searched_element_list_4_iso'
      call set_search_mesh_list_4_iso(numnod, numele, numsurf, numedge, &
     &    nnod_4_edge, ie_edge, isf_4_ele, iedge_4_sf, interior_ele,    &
     &    inod_smp_stack, iele_smp_stack, isurf_smp_stack,              &
     &    iedge_smp_stack, num_mat, num_mat_bc,  mat_istack, mat_item)
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_constant_4_ref_iso'
      call allocate_constant_4_ref_iso(num_iso, numnod,               &
     &    nele_search_iso_tot)
      if (iflag_debug.eq.1) write(*,*) 'allocate_nnod_iso'
      call allocate_nnod_iso(np_smp, num_iso, numnod, numedge)
      if (iflag_debug.eq.1) write(*,*) 'allocate_num_patch_iso'
      call allocate_num_patch_iso(np_smp, num_iso)
!
      call allocate_iso_outputs_num(nprocs, my_rank, num_iso)
!
      end subroutine isosurface_init
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine isosurface_main(istep_iso,                             &
     &          numnod, numele, numedge, nnod_4_ele, nnod_4_edge,       &
     &          ie, ie_edge, iedge_4_ele, globalnodid,                  &
     &          xx, radius, a_radius, s_cylinder, a_s_cylinder,         &
     &          inod_smp_stack, num_nod_phys, num_tot_nod_phys,         &
     &          istack_nod_component, d_nod)
!
!
      use m_geometry_constants
      use m_control_params_4_iso
      use m_geometry_list_4_iso
      use m_patch_data_iso
      use m_iso_outputs
!
      use set_const_4_sections
      use find_node_and_patch_psf
      use set_fields_for_psf
      use output_section_files
      use collect_psf_data
!
      integer(kind = kint), intent(in) :: istep_iso
!
      integer(kind=kint), intent(in) :: numnod, numele, numedge
      integer(kind=kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind=kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind=kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind=kint), intent(in) :: iedge_4_ele(numele,nedge_4_ele)
!
      integer(kind=kint), intent(in) :: globalnodid(numnod)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      real(kind = kreal), intent(in) :: s_cylinder(numnod)
      real(kind = kreal), intent(in) :: a_s_cylinder(numnod)
!
      integer(kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      integer(kind = kint), intent(in) :: num_tot_nod_phys
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_nod_component(0:num_nod_phys)
      real(kind = kreal), intent(in)  :: d_nod(numnod,num_tot_nod_phys)
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_const_4_isosurfaces'
      call set_const_4_isosurfaces(numnod, inod_smp_stack,              &
     &    xx, radius, a_radius, s_cylinder, a_s_cylinder,               &
     &    num_nod_phys, num_tot_nod_phys, istack_nod_component, d_nod)
!
      if (iflag_debug.eq.1) write(*,*) 'set_node_and_patch_iso'
      call set_node_and_patch_iso(numnod, numele, numedge, nnod_4_ele,  &
     &    nnod_4_edge, globalnodid, xx, ie, ie_edge, iedge_4_ele,       &
     &    num_nod_phys, num_tot_nod_phys, istack_nod_component, d_nod)
!
      if (iflag_debug.eq.1) write(*,*) 'set_field_4_iso'
      call set_field_4_iso(numnod, numedge, nnod_4_edge, ie_edge,       &
     &    num_nod_phys, num_tot_nod_phys, istack_nod_component, d_nod)
!
      call deallocate_inod_iso
!
!
      if (iflag_debug.eq.1) write(*,*) 'collect_numbers_4_iso'
      call collect_numbers_4_iso
!
!
      call allocate_iso_outputs_data(my_rank,num_iso)
      call allocate_SR_array_iso(my_rank, nprocs,                       &
     &    nnod_iso_tot, npatch_tot_iso_smp)
!
!
      if (iflag_debug.eq.1) write(*,*) 'collect_data_4_iso'
      call collect_data_4_iso
!
      call time_prog_barrier
!
      if (iflag_debug.eq.1) write(*,*) 'output_iso_ucds'
      call output_iso_ucds(istep_iso)
!
!
      call deallocate_SR_array_iso(my_rank)
      call deallocate_iso_outputs_data(my_rank, num_iso)
      call deallocate_dat_on_patch_iso
      call deallocate_patch_data_iso
      call deallocate_position_iso
!
      end subroutine isosurface_main
!
!  ---------------------------------------------------------------------
!
      end module isosurface
