!>@file   set_fields_at_seed_points.f90
!!@brief  module set_fields_at_seed_points
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine set_field_at_each_seed_point(node, ele, nod_fld,     &
!!     &          fline_fields, iphys_4_fline, iele_seed, x4_seed,      &
!!     &          v_fline_start, c_fline_start)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ctl_params_viz_fields), intent(in) :: fline_fields
!!        integer(kind = kint), intent(in) :: iphys_4_fline
!!        integer(kind = kint), intent(in) :: iele_seed(1)
!!        real(kind = kreal), intent(in) :: x4_seed(4)
!!        real(kind = kreal), intent(inout) :: v_fline_start(4)
!!        real(kind = kreal), intent(inout)                             &
!!     &         :: c_fline_start(fline_fields%ntot_color_comp)
!!
!!      subroutine find_seed_point_in_each_ele                          &
!!     &         (node, ele, xx_surf_start_fline, idx_fln_dist,         &
!!     &          num_search, itp_ele_work, ip_surf_start_fline,        &
!!     &          iele_surf_start_fline, xi_surf_start_fline,           &
!!     &          ierr_inter)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        real(kind = kreal), intent(in) :: xx_surf_start_fline(3)
!!        integer(kind = kint), intent(in) :: idx_fln_dist(ele%numele)
!!        integer(kind = kint), intent(in) :: num_search
!!        type(cal_interpolate_coefs_work), intent(inout) :: itp_ele_work
!!        integer(kind = kint), intent(inout) :: ip_surf_start_fline
!!        integer(kind = kint), intent(inout) :: iele_surf_start_fline
!!        real(kind = kreal), intent(inout) :: xi_surf_start_fline(3)
!!        integer(kind = kint), intent(inout) :: ierr_inter
!!@endverbatim
!
      module set_fields_at_seed_points
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
      use t_phys_data
      use t_control_params_4_fline
      use t_source_of_filed_line
      use t_tracing_data
!
      implicit none
!
      integer(kind = kint), parameter, private :: maxitr = 20
      real(kind = kreal), parameter, private ::   eps_iter = 1.0d-9
      integer(kind = kint), parameter, private :: iflag_nomessage = 0
      real(kind = kreal), parameter, private ::   error_level = 1.0d-9
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_field_at_each_seed_point(node, ele, nod_fld,       &
     &          fline_fields, iphys_4_fline, iele_seed, x4_seed,        &
     &          v_fline_start, c_fline_start)
!
      use t_find_interpolate_in_ele
      use field_at_each_seed_point
      use tracer_field_interpolate
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      type(ctl_params_viz_fields), intent(in) :: fline_fields
      integer(kind = kint), intent(in) :: iphys_4_fline
!
      integer(kind = kint), intent(in) :: iele_seed(1)
      real(kind = kreal), intent(in) :: x4_seed(4)
!
      real(kind = kreal), intent(inout) :: v_fline_start(4)
      real(kind = kreal), intent(inout)                                 &
     &         :: c_fline_start(fline_fields%ntot_color_comp)
!
      type(cal_interpolate_coefs_work), save :: itp_ele_work_f
      integer(kind = kint) :: ierr_inter
      real(kind = kreal) :: xi_in_ele(3)
!
!
      call alloc_work_4_interpolate(ele%nnod_4_ele, itp_ele_work_f)
      xi_in_ele(1:3) = -2.0
      call find_interpolate_in_ele(x4_seed, maxitr, eps_iter,           &
     &    my_rank, iflag_nomessage, error_level, node, ele,             &
     &    iele_seed(1), itp_ele_work_f, xi_in_ele, ierr_inter)
      call dealloc_work_4_interpolate(itp_ele_work_f)
!
      call cal_each_seed_velocity_in_ele(ele,                           &
     &    nod_fld%n_point, nod_fld%d_fld(1,iphys_4_fline),              &
     &    iele_seed, xi_in_ele, v_fline_start)
      call cal_fields_in_element(iele_seed, xi_in_ele, x4_seed,         &
     &    ele, nod_fld, fline_fields, c_fline_start)
!
      end subroutine set_field_at_each_seed_point
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine find_seed_point_in_each_ele                            &
     &         (node, ele, xx_surf_start_fline, idx_fln_dist,           &
     &          num_search, itp_ele_work, ip_surf_start_fline,          &
     &          iele_surf_start_fline, xi_surf_start_fline,             &
     &          ierr_inter)
!
      use t_find_interpolate_in_ele
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      real(kind = kreal), intent(in) :: xx_surf_start_fline(3)
      integer(kind = kint), intent(in) :: idx_fln_dist(ele%numele)
      integer(kind = kint), intent(in) :: num_search
!
      type(cal_interpolate_coefs_work), intent(inout) :: itp_ele_work
      integer(kind = kint), intent(inout) :: ip_surf_start_fline
      integer(kind = kint), intent(inout) :: iele_surf_start_fline
      real(kind = kreal), intent(inout) :: xi_surf_start_fline(3)
      integer(kind = kint), intent(inout) :: ierr_inter
!
      real(kind = kreal) :: xi(3)
      integer(kind = kint) :: iele, inum
!
!
      ip_surf_start_fline = -1
      iele_surf_start_fline = 0
      xi_surf_start_fline(1:3) = -2.0
      do inum = 1, num_search
        iele = idx_fln_dist(inum)
        if(ele%interior_ele(iele) .le. 0) cycle
!
        ierr_inter = 0
        xi(1:3) = -2.0
        call find_interpolate_in_ele                                    &
     &     (xx_surf_start_fline(1), maxitr, eps_iter,                   &
     &      my_rank, iflag_nomessage, error_level,                      &
     &      node, ele, iele, itp_ele_work, xi, ierr_inter)
        if(ierr_inter.gt.1 .and. ierr_inter.le.maxitr) then
          ip_surf_start_fline =      my_rank
          iele_surf_start_fline =    iele
          xi_surf_start_fline(1:3) = xi(1:3)
          exit
        end if
      end do
!
      end subroutine find_seed_point_in_each_ele
!
!  ---------------------------------------------------------------------
!
      end module set_fields_at_seed_points
