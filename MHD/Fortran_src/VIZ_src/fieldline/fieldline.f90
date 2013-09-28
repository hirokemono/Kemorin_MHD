!
!      module fieldline
!
!      Written by H. Matsui on July, 2006
!
!      subroutine field_line_init(numnod, numele, e_multi,              &
!     &    num_mat, num_mat_bc, mat_name, mat_istack, mat_item,         &
!     &    num_surf, num_surf_bc, surf_name, surf_istack, surf_item,    &
!     &    num_nod_phys, phys_nod_name)
!
!      subroutine field_line_main(istep_psf, numnod, numele, numsurf,   &
!     &       nnod_4_surf, inod_smp_stack, globalnodid,                 &
!     &       xx, radius, a_radius, s_cylinder, a_s_cylinder,           &
!     &       globalelmid, e_multi, ie_surf, isf_4_ele, iele_4_surf,    &
!     &       x_surf, vnorm_surf, area_surf, interior_surf,             &
!     &       num_mat, num_mat_bc, mat_istack,  mat_item,               &
!     &       ntot_ele_4_node, iele_stack_4_node, iele_4_node,          &
!     &       num_neib, ntot_import, ntot_export, id_neib,              &
!     &       istack_import, istack_export, item_import, item_export,   &
!     &       num_nod_phys, num_tot_nod_phys, istack_nod_component,     &
!     &       d_nod)
!
      module fieldline
!
      use m_precision
!
      use m_machine_parameter
      use m_control_params_4_fline
      use m_geometry_constants
      use m_global_fline
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine field_line_init(numnod, numele, e_multi,               &
     &    num_mat, num_mat_bc, mat_name, mat_istack, mat_item,          &
     &    num_surf, num_surf_bc, surf_name, surf_istack, surf_item,     &
     &    num_nod_phys, phys_nod_name)
!
      use calypso_mpi
      use m_source_4_filed_line
      use m_local_fline
      use set_fline_control
!
      integer(kind=kint), intent(in) :: numnod, numele
      real(kind = kreal), intent(in) :: e_multi(numele)
!
      integer(kind=kint), intent(in) :: num_mat, num_mat_bc
      integer(kind=kint), intent(in) :: mat_istack(0:num_mat)
      integer(kind=kint), intent(in) :: mat_item(num_mat_bc)
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind=kint), intent(in) :: num_surf, num_surf_bc
      integer(kind=kint), intent(in) :: surf_istack(0:num_surf)
      integer(kind=kint), intent(in) :: surf_item(2,num_surf_bc)
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
!
      if (iflag_debug.eq.1) write(*,*) 's_set_fline_control'
      call s_set_fline_control(numele, e_multi,                         &
     &    num_mat, num_mat_bc, mat_name, mat_istack, mat_item,          &
     &    num_surf, num_surf_bc, surf_name, surf_istack, surf_item,     &
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
     &       nnod_4_surf, inod_smp_stack, globalnodid,                  &
     &       xx, radius, a_radius, s_cylinder, a_s_cylinder,            &
     &       globalelmid, e_multi, ie_surf, isf_4_ele, iele_4_surf,     &
     &       x_surf, vnorm_surf, area_surf, interior_surf,              &
     &       num_mat, num_mat_bc, mat_istack,  mat_item,                &
     &       ntot_ele_4_node, iele_stack_4_node, iele_4_node,           &
     &       num_neib, ntot_import, ntot_export, id_neib,               &
     &       istack_import, istack_export, item_import, item_export,    &
     &       num_nod_phys, num_tot_nod_phys, istack_nod_component,      &
     &       d_nod)
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
      integer(kind = kint), intent(in) :: globalnodid(numnod)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      real(kind = kreal), intent(in) :: s_cylinder(numnod)
      real(kind = kreal), intent(in) :: a_s_cylinder(numnod)
!
      integer(kind = kint), intent(in) :: globalelmid(numele)
      real(kind = kreal), intent(in) :: e_multi(numele)
!
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
      integer(kind = kint), intent(in) :: interior_surf(numsurf)
      real(kind = kreal), intent(in) :: x_surf(numsurf,3)
      real(kind = kreal), intent(in) :: vnorm_surf(numsurf,3)
      real(kind = kreal), intent(in) :: area_surf(numsurf)
!
      integer(kind=kint), intent(in) :: num_mat, num_mat_bc
      integer(kind=kint), intent(in) :: mat_istack(0:num_mat)
      integer(kind=kint), intent(in) :: mat_item(num_mat_bc)
!
      integer (kind=kint), intent(in) :: ntot_ele_4_node
      integer (kind=kint), intent(in) :: iele_stack_4_node(0:numnod)
      integer (kind=kint), intent(in) :: iele_4_node(ntot_ele_4_node)
!
      integer(kind = kint), intent(in) :: num_neib, ntot_import
      integer(kind = kint), intent(in) :: id_neib(num_neib)
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in) :: item_import(ntot_import)
!
      integer(kind = kint), intent(in) :: ntot_export
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
      integer(kind = kint), intent(in) :: item_export(ntot_export)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      integer(kind = kint), intent(in) :: num_tot_nod_phys
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_nod_component(0:num_nod_phys)
      real(kind = kreal), intent(in)  :: d_nod(numnod,num_tot_nod_phys)
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
     &        numnod, numele, numsurf, nnod_4_surf,                     &
     &        globalelmid, e_multi, ie_surf, isf_4_ele, iele_4_surf,    &
     &        x_surf, vnorm_surf, area_surf,                            &
     &        num_mat, num_mat_bc, mat_istack,  mat_item)
      end do
!
      do i_fln = 1, num_fline
        if (iflag_debug.eq.1) write(*,*) 's_const_field_lines', i_fln
        call s_const_field_lines(i_fln, numnod, numele, numsurf,        &
     &          nnod_4_surf, globalnodid, xx, globalelmid, ie_surf,     &
     &          isf_4_ele, iele_4_surf, interior_surf, vnorm_surf,      &
     &          ntot_ele_4_node, iele_stack_4_node, iele_4_node,        &
     &          num_neib, ntot_import, ntot_export, id_neib,            &
     &          istack_import, istack_export, item_import, item_export)
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
