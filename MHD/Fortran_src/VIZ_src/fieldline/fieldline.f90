!>@file   fieldline.f90
!!@brief  module fieldline
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine field_line_init(numnod, numele, interior_ele,        &
!!     &          ele_grp, sf_grp,  num_nod_phys, phys_nod_name)
!!
!!
!!      subroutine field_line_main(istep_psf, numnod, numele, numsurf,  &
!!     &      nnod_4_surf, inod_smp_stack, inod_global,                 &
!!     &      xx, radius, a_radius, s_cylinder, a_s_cylinder,           &
!!     &      iele_global, interior_ele, ie_surf, isf_4_ele,            &
!!     &      iele_4_surf, x_surf, vnorm_surf, area_surf, interior_surf,&
!!     &      ele_grp, ele_4_nod,  num_nod_phys, num_tot_nod_phys,      &
!!     &      istack_nod_component, d_nod, nod_comm)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(group_data), intent(in) :: ele_grp
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(element_around_node), intent(in) :: ele_4_nod
!!@endverbatim
!
      module fieldline
!
      use m_precision
!
      use m_machine_parameter
      use m_control_params_4_fline
      use m_geometry_constants
      use m_global_fline
      use t_comm_table
      use t_group_data
      use t_next_node_ele_4_node
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine field_line_init(numnod, numele, interior_ele,          &
     &          ele_grp, sf_grp,  num_nod_phys, phys_nod_name)
!
      use calypso_mpi
      use m_source_4_filed_line
      use m_local_fline
      use set_fline_control
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: interior_ele(numele)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: sf_grp
!
!
      if (iflag_debug.eq.1) write(*,*) 's_set_fline_control'
      call s_set_fline_control(numele, interior_ele,                    &
     &    ele_grp%num_grp, ele_grp%num_item, ele_grp%grp_name,          &
     &    ele_grp%istack_grp, ele_grp%item_grp,                         &
     &    sf_grp%num_grp, sf_grp%num_item, sf_grp%grp_name,             &
     &    sf_grp%istack_grp, sf_grp%item_sf_grp,                        &
     &    num_nod_phys, phys_nod_name)
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_local_data_4_fline'
      call allocate_local_data_4_fline(numnod)
      call allocate_start_point_fline
      call allocate_num_gl_start_fline(nprocs)
      call allocate_local_fline
      call allocate_global_fline_num
!
      end subroutine field_line_init
!
!  ---------------------------------------------------------------------
!
      subroutine field_line_main(istep_psf, numnod, numele, numsurf,    &
     &      nnod_4_surf, inod_smp_stack, inod_global,                   &
     &      xx, radius, a_radius, s_cylinder, a_s_cylinder,             &
     &      iele_global, interior_ele, ie_surf, isf_4_ele,              &
     &      iele_4_surf, x_surf, vnorm_surf, area_surf, interior_surf,  &
     &      ele_grp, ele_4_nod,  num_nod_phys, num_tot_nod_phys,        &
     &      istack_nod_component, d_nod, nod_comm)
!
      use set_fields_for_fieldline
      use const_field_lines
      use collect_fline_data
!
      integer(kind = kint), intent(in) :: istep_psf
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      real(kind = kreal), intent(in) :: s_cylinder(numnod)
      real(kind = kreal), intent(in) :: a_s_cylinder(numnod)
!
      integer(kind = kint_gl), intent(in) :: iele_global(numele)
      integer(kind=kint), intent(in) :: interior_ele(numele)
!
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
      integer(kind = kint), intent(in) :: interior_surf(numsurf)
      real(kind = kreal), intent(in) :: x_surf(numsurf,3)
      real(kind = kreal), intent(in) :: vnorm_surf(numsurf,3)
      real(kind = kreal), intent(in) :: area_surf(numsurf)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      integer(kind = kint), intent(in) :: num_tot_nod_phys
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_nod_component(0:num_nod_phys)
      real(kind = kreal), intent(in)  :: d_nod(numnod,num_tot_nod_phys)
!
      type(communication_table), intent(in) :: nod_comm
      type(group_data), intent(in) :: ele_grp
      type(element_around_node), intent(in) :: ele_4_nod
!
      integer(kind = kint) :: i_fln
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_local_field_4_fline'
      call set_local_field_4_fline(numnod, inod_smp_stack,              &
     &          xx, radius, a_radius, s_cylinder, a_s_cylinder,         &
     &          num_nod_phys, num_tot_nod_phys, istack_nod_component,   &
     &          d_nod)
!
      do i_fln = 1, num_fline
        if (iflag_debug.eq.1) write(*,*) 's_set_fields_for_fieldline'
        call s_set_fields_for_fieldline(i_fln,                          &
     &      numnod, numele, numsurf, nnod_4_surf, iele_global,          &
     &      interior_ele, ie_surf, isf_4_ele, iele_4_surf,              &
     &      x_surf, vnorm_surf, area_surf,                              &
     &      ele_grp%num_grp, ele_grp%num_item, ele_grp%istack_grp,      &
     &      ele_grp%item_grp)
      end do
!
      do i_fln = 1, num_fline
        if (iflag_debug.eq.1) write(*,*) 's_const_field_lines', i_fln
        call s_const_field_lines(i_fln, numnod, numele, numsurf,        &
     &          nnod_4_surf, inod_global, xx, iele_global, ie_surf,     &
     &          isf_4_ele, iele_4_surf, interior_surf, vnorm_surf,      &
     &          ele_4_nod%ntot, ele_4_nod%istack_4_node,                &
     &          ele_4_nod%iele_4_node, nod_comm)
!
        if (iflag_debug.eq.1) write(*,*) 's_collect_fline_data', i_fln
       call s_collect_fline_data(istep_psf, i_fln)
      end do
!
      end subroutine field_line_main
!
!  ---------------------------------------------------------------------
!
      end module fieldline
