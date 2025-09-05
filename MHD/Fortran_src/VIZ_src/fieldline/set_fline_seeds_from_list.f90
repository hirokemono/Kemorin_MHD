!>@file   set_fline_seeds_from_list.f90
!!@brief  module set_fline_seeds_from_list
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine init_FLINE_seed_from_list(i_fln, node, ele,          &
!!     &          fln_prm, fln_src, fln_dist, num_line_local)
!!        integer(kind = kint), intent(in) :: i_fln
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!        type(each_fieldline_source), intent(inout) :: fln_src
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!        type(FLINE_element_size), intent(inout) :: fln_dist
!!        integer(kind = kint), intent(inout) :: num_line_local
!!      subroutine set_FLINE_seed_field_from_list                       &
!!     &         (node, ele, nod_fld, fln_prm, fln_src, fln_tce)
!!         type(node_data), intent(in) :: node
!!         type(element_data), intent(in) :: ele
!!         type(phys_data), intent(in) :: nod_fld
!!         type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!
!!      subroutine count_FLINE_seed_from_list(num_line_local,           &
!!     &                                      fln_prm, fln_tce)
!!        integer(kind = kint), intent(in) :: num_line_local
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!
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
!!@endverbatim
!
      module set_fline_seeds_from_list
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
      subroutine const_FLINE_seed_from_list(node, ele, nod_fld,         &
     &          fln_prm, fln_src, num_line_local, fln_tce)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(in) :: fln_src
      integer(kind = kint), intent(in) :: num_line_local
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
!
      call count_FLINE_seed_from_list(num_line_local, fln_prm, fln_tce)
      call set_FLINE_seed_field_from_list(node, ele, nod_fld, fln_prm,  &
     &                                    fln_src, fln_tce)
      if(i_debug .gt. 0) call check_line_start_fline(fln_tce)
!
      end subroutine const_FLINE_seed_from_list
!
!  ---------------------------------------------------------------------
!
      subroutine init_FLINE_seed_from_list(i_fln, node, ele,            &
     &          fln_prm, fln_src, fln_dist, num_line_local)
!
      use calypso_mpi_int
      use t_control_data_flines
      use t_find_interpolate_in_ele
      use field_at_each_seed_point
      use set_fline_control
      use quicksort
!
      integer(kind = kint), intent(in) :: i_fln
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(fieldline_paramter), intent(inout) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
      type(FLINE_element_size), intent(inout) :: fln_dist
      integer(kind = kint), intent(inout) :: num_line_local
!
      integer(kind = kint) :: ierr_inter
      integer(kind = kint) :: num_search
!
      integer(kind = kint) :: i
!
!
      do i = 1, fln_prm%num_each_field_line
        call seed_distance_from_ele_center                              &
     &     (ele, fln_prm%xx_surf_start_fline(1,i), fln_dist%ele_size,   &
     &      fln_dist%index, fln_dist%distance, num_search)
!
        if(num_search .gt. 1) then
          call quicksort_real_w_index(ele%numele, fln_dist%distance(1), &
     &        ione, num_search, fln_dist%index(1))
        end if
!
        call find_seed_point_in_each_ele                                &
     &     (node, ele, fln_prm%xx_surf_start_fline(1,i),                &
     &      fln_dist%index, num_search, fln_dist%itp_ele_work_f,        &
     &      fln_src%ip_surf_start_fline(i),                             &
     &      fln_src%iele_surf_start_fline(i),                           &
     &      fln_src%xi_surf_start_fline(1:3,i), ierr_inter)
      end do
!
      num_line_local = 0
      do i = 1, fln_prm%num_each_field_line
      if(fln_src%ip_surf_start_fline(i) .eq. my_rank)                   &
        num_line_local = num_line_local + 1
      end do
!
      write(*,*) my_rank, i_fln, 'ierr_inter ', ierr_inter
      call check_each_fieldline_source(i_fln, ele%numele, fln_src)
!
      end subroutine init_FLINE_seed_from_list
!
!  ---------------------------------------------------------------------
!
      subroutine count_FLINE_seed_from_list(num_line_local,             &
     &                                      fln_prm, fln_tce)
!
      use calypso_mpi_int
!
      integer(kind = kint), intent(in) :: num_line_local
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: i
!
!
      fln_tce%num_current_fline = num_line_local
      if(fln_prm%id_fline_direction .eq. iflag_both_trace) then
        fln_tce%num_current_fline = 2 * fln_tce%num_current_fline
      end if
      call resize_line_start_fline(fln_tce%num_current_fline,           &
     &                             fln_prm%fline_fields, fln_tce)
!
      fln_tce%istack_current_fline(0) = 0
      call calypso_mpi_allgather_one_int(fln_tce%num_current_fline,     &
     &                                 fln_tce%istack_current_fline(1))
      do i = 1, nprocs
        fln_tce%istack_current_fline(i)                                 &
     &     = fln_tce%istack_current_fline(i-1)                          &
     &      + fln_tce%istack_current_fline(i)
      end do
!
      end subroutine count_FLINE_seed_from_list
!
!  ---------------------------------------------------------------------
!
      subroutine set_FLINE_seed_field_from_list                         &
     &         (node, ele, nod_fld, fln_prm, fln_src, fln_tce)
!
      use sel_interpolate_scalar
      use extend_field_line
      use trace_in_element
      use tracer_field_interpolate
      use field_at_each_seed_point
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(in) :: fln_src
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: icou, inum
      integer(kind = kint) :: ip
!
!
      icou = 0
      do inum = 1, fln_prm%num_each_field_line
          if(fln_src%ip_surf_start_fline(inum) .ne. my_rank) cycle
          icou = icou + 1
!
          call cal_each_seed_field_in_ele(node, ele, nod_fld,           &
     &        fln_prm%fline_fields, fln_prm%iphys_4_fline,              &
     &        fln_src%iele_surf_start_fline(inum),                      &
     &        fln_src%xi_surf_start_fline(1,inum),                      &
     &        fln_prm%xx_surf_start_fline(1,inum),                      &
     &        fln_tce%v_fline_start(1,icou),                            &
     &        fln_tce%c_fline_start(1,icou))
!
!
          fln_tce%isf_dbl_start(1,icou) = my_rank
          fln_tce%isf_dbl_start(2,icou)                                 &
     &      = fln_src%iele_surf_start_fline(inum)
          fln_tce%isf_dbl_start(3,icou)                                 &
     &            = surface_mode_in_each_ele(error_level,               &
     &             fln_src%xi_surf_start_fline(1,inum))
!
          fln_tce%iline_original(icou) = inum
          fln_tce%xx_fline_start(1:3,icou)                              &
     &         = fln_prm%xx_surf_start_fline(1:3,inum)
          fln_tce%xx_fline_start(4,icou) = one
          fln_tce%trace_length(icou) = 0.0d0
          fln_tce%icount_fline(icou) = 0
          
          if     (fln_prm%id_fline_direction                            &
     &                  .eq. iflag_forward_trace) then
           fln_tce%iflag_direction(icou) = 1
          else if(fln_prm%id_fline_direction                            &
     &                  .eq. iflag_backward_trace) then

            fln_tce%iflag_direction(icou) = -1
          else
            fln_tce%iflag_direction(icou) = 1
!
            icou = icou + 1
            fln_tce%iflag_direction(icou) = -1
            fln_tce%isf_dbl_start(1,icou) = my_rank
            fln_tce%isf_dbl_start(2,icou)                               &
     &            = fln_src%iele_surf_start_fline(inum)
            fln_tce%isf_dbl_start(3,icou)                               &
     &            = surface_mode_in_each_ele(error_level,               &
     &             fln_src%xi_surf_start_fline(1,inum))
!
            fln_tce%trace_length(icou) = 0.0d0
            fln_tce%icount_fline(icou) = 0
            call copy_global_start_fline(icou, (icou-1),                &
     &                                   fln_prm%fline_fields, fln_tce)

          end if
        end do
!
      end subroutine set_FLINE_seed_field_from_list
!
!  ---------------------------------------------------------------------
!
      subroutine set_field_at_each_seed_point(node, ele, nod_fld,       &
     &          fline_fields, iphys_4_fline, iele_seed, x4_seed,        &
     &          v_fline_start, c_fline_start)
!
      use t_find_interpolate_in_ele
      use field_at_each_seed_point
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
      call alloc_work_4_interpolate(ele%nnod_4_ele, itp_ele_work_f)
      xi_in_ele(1:3) = -2.0
      call find_interpolate_in_ele(x4_seed, maxitr, eps_iter,           &
     &    my_rank, iflag_nomessage, error_level, node, ele,             &
     &    iele_seed(1), itp_ele_work_f, xi_in_ele, ierr_inter)
      call dealloc_work_4_interpolate(itp_ele_work_f)
!
      call cal_each_seed_field_in_ele                                   &
     &   (node, ele, nod_fld, fline_fields, iphys_4_fline,              &
     &    iele_seed, xi_in_ele, x4_seed, v_fline_start, c_fline_start)
!
      end subroutine set_field_at_each_seed_point
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine find_seed_point_in_each_ele                            &
     &         (node, ele, xx_surf_start_fline, idx_fln_dist,           &
     &          num_search, itp_ele_work_f, ip_surf_start_fline,        &
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
      type(cal_interpolate_coefs_work), intent(inout) :: itp_ele_work_f
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
     &      node, ele, iele, itp_ele_work_f, xi, ierr_inter) 
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
      end module set_fline_seeds_from_list
