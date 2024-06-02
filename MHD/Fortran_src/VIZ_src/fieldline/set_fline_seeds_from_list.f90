!>@file   set_fline_seeds_from_list.f90
!!@brief  module set_fline_seeds_from_list
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine FLINE_initialize                                     &
!!     &         (increment_fline, fem, nod_fld, fline_ctls, fline)
!!      subroutine FLINE_visualize                                      &
!!     &         (istep_fline, time_d, fem, next_tbl, nod_fld, fline)
!!      subroutine FLINE_finalize(fline)
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: fem
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_controls), intent(inout) :: fline_ctls
!!        type(fieldline_module), intent(inout) :: fline
!!      subroutine set_FLINE_seed_field_from_list                       &
!!     &         (node, ele, nod_fld, fln_prm, fln_src, fln_tce)
!!         type(node_data), intent(in) :: node
!!         type(element_data), intent(in) :: ele
!!         type(phys_data), intent(in) :: nod_fld
!!         type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
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
      real(kind = kreal), parameter, private ::   error_level = 0.0
!
      type FLINE_element_size
        real(kind = kreal), allocatable :: ele_size(:)
        real(kind = kreal), allocatable :: distance(:)
        integer(kind = kint), allocatable :: index(:)
      end type FLINE_element_size
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_FLINE_element_size(ele, fln_dist)
      type(element_data), intent(in) :: ele
      type(FLINE_element_size), intent(inout) :: fln_dist
!
      allocate(fln_dist%ele_size(ele%numele))
      allocate(fln_dist%distance(ele%numele))
      allocate(fln_dist%index(ele%numele))
!
      if(ele%numele .le. 0) return
!$omp parallel workshare 
      fln_dist%ele_size(1:ele%numele) = 0.0d0
      fln_dist%distance(1:ele%numele) = 0.0d0
      fln_dist%index(1:ele%numele) =    0
!$omp end parallel workshare 
!
      end subroutine alloc_FLINE_element_size
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_FLINE_element_size(fln_dist)
      type(FLINE_element_size), intent(inout) :: fln_dist
!
      if(allocated(fln_dist%ele_size) .eqv. .FALSE.) return
      deallocate(fln_dist%ele_size)
      deallocate(fln_dist%distance, fln_dist%index)
!
      end subroutine dealloc_FLINE_element_size
!
!  ---------------------------------------------------------------------
!
      subroutine cal_FLINE_element_size(node, ele, fln_dist)
!
      use m_connect_hexa_2_tetra
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FLINE_element_size), intent(inout) :: fln_dist
!
      real(kind = kreal) :: x(ele%nnod_4_ele)
      real(kind = kreal) :: y(ele%nnod_4_ele)
      real(kind = kreal) :: z(ele%nnod_4_ele)
      real(kind = kreal) :: size_max(3)
      integer(kind = kint) :: inod, iele, k1
!
      if(ele%nnod_4_ele .eq. num_t_linear) then
        call set_1_hexa_2_5_tetra
      else if (ele%nnod_4_ele .eq. num_t_quad) then
        call set_1_hexa_2_21_tetra
      else if (ele%nnod_4_ele .eq. num_t_lag) then
        call set_1_hexa_2_40_tetra
      end if
!
!$omp parallel do private(iele,k1,inod,x,y,z)
      do iele = 1, ele%numele
        do k1 = 1, ele%nnod_4_ele
          inod = ele%ie(iele,k1)
          x(k1) = node%xx(inod,1)
          y(k1) = node%xx(inod,2)
          z(k1) = node%xx(inod,3)
        end do
        size_max(1) = maxval(x) - minval(x)
        size_max(2) = maxval(y) - minval(y)
        size_max(3) = maxval(z) - minval(z)
        fln_dist%ele_size(iele) = sqrt(size_max(1)*size_max(1)          &
     &                               + size_max(2)*size_max(2)          &
     &                               + size_max(3)*size_max(3))
      end do
!$omp end parallel do
!
      end subroutine cal_FLINE_element_size
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_FLINE_seed_from_list(node, ele,                   &
     &          fln_prm, fln_src, fln_tce, fln_dist)
!
      use calypso_mpi_int
      use t_control_data_flines
      use t_find_interpolate_in_ele
      use set_fline_control
      use quicksort
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(fieldline_paramter), intent(inout) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
      type(each_fieldline_trace), intent(inout) :: fln_tce
      type(FLINE_element_size), intent(inout) :: fln_dist
!
      type(cal_interpolate_coefs_work), save :: itp_ele_work_f
      integer(kind = kint) :: ierr_inter
!
      real(kind = kreal) :: x, y, z
      real(kind = kreal) :: dist_tmp
      real(kind = kreal) :: xi(3)
      integer(kind = kint) :: i, iele, inum
      integer(kind = kint) :: num_search
!      integer(kind = kint) :: ip, i_fln

!
      call alloc_work_4_interpolate(ele%nnod_4_ele, itp_ele_work_f)
!
      do i = 1, fln_prm%num_each_field_line
        x = fln_prm%xx_surf_start_fline(1,i)
        y = fln_prm%xx_surf_start_fline(2,i)
        z = fln_prm%xx_surf_start_fline(3,i)
        num_search = 0
        do iele = 1, ele%numele
          dist_tmp = sqrt ((x - ele%x_ele(iele,1))**2                   &
     &                   + (y - ele%x_ele(iele,2))**2                   &
     &                   + (z - ele%x_ele(iele,3))**2)
          if(dist_tmp .le. fln_dist%ele_size(iele)) then
            num_search = num_search + 1
            fln_dist%index(num_search) =    iele
            fln_dist%distance(num_search) = dist_tmp
          end if
        end do

        if(num_search .gt. 1) then
          call quicksort_real_w_index(ele%numele,                       &
    &         fln_dist%distance(1), ione, num_search, fln_dist%index(1))
        end if
!
        fln_src%ip_surf_start_fline(i) = -1
        fln_src%iele_surf_start_fline(i) = 0
        fln_src%xi_surf_start_fline(1:3,i) = -2.0
        do inum = 1, num_search
          iele = fln_dist%index(inum)
          if(ele%ie(iele,1) .gt. node%internal_node) cycle
          ierr_inter = 0
          xi(1:3) = -2.0
          call find_interpolate_in_ele                                  &
     &       (fln_prm%xx_surf_start_fline(1,i), maxitr, eps_iter,       &
     &        my_rank, iflag_nomessage, error_level,                    &
     &        node, ele, iele, itp_ele_work_f, xi, ierr_inter)  
          if(ierr_inter.gt.1 .and. ierr_inter.le.maxitr) then
            fln_src%ip_surf_start_fline(i) = my_rank
            fln_src%iele_surf_start_fline(i) = iele
            fln_src%xi_surf_start_fline(1:3,i) = xi(1:3)
            exit
          end if
        end do
      end do
        
!      do ip = 1, nprocs
!        call calypso_mpi_barrier
!        if(my_rank .ne. ip-1) cycle
!        do i = 1, fln_prm%num_each_field_line
!          if(fln_src%ip_surf_start_fline(i) .ge. 0) then
!          write(*,*) my_rank, i_fln, i, 'fln_prm',                     &
!     &              fln_src%ip_surf_start_fline(i),                    &
!     &              fln_src%iele_surf_start_fline(i),                  &
!     &              fln_src%xi_surf_start_fline(1:3,i),                &
!     &              ele%numele, ierr_inter
!          end if
!        end do
!      end do
!
!        call calypso_mpi_barrier
      fln_src%num_line_local = 0
      do i = 1, fln_prm%num_each_field_line
      if(fln_src%ip_surf_start_fline(i) .eq. my_rank)                   &
        fln_src%num_line_local = fln_src%num_line_local + 1
      end do
!        call calypso_mpi_barrier
!        write(*,*) my_rank, 'fln_src%num_line_local',                  &
!     &            fln_src%num_line_local

      fln_tce%num_current_fline = fln_src%num_line_local
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
      call dealloc_work_4_interpolate(itp_ele_work_f)
!
      end subroutine init_FLINE_seed_from_list
!
!  ---------------------------------------------------------------------
!
      subroutine set_FLINE_seed_field_from_list                         &
     &         (node, ele, nod_fld, fln_prm, fln_src, fln_tce)
!
      use sel_interpolate_scalar
      use extend_field_line
      use trace_in_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(in) :: fln_src
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: icou, inum
!
      integer(kind = kint) :: istack_tbl_wtype_smp(0:4) = (/0,0,0,0,1/)
!      real(kind = kreal) :: position_check(3)
! 
      icou = 0
      do inum = 1, fln_prm%num_each_field_line
          if(fln_src%ip_surf_start_fline(inum) .ne. my_rank) cycle
          icou = icou + 1
!
          call s_sel_interpolate_scalar_ele                             &
     &         (1, node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,     &
     &          nod_fld%d_fld(1,fln_prm%iphys_4_fline),                 &
     &          istack_tbl_wtype_smp(3), ione,                          &
     &          fln_src%iele_surf_start_fline(inum),                    &
     &          fln_src%xi_surf_start_fline(1,inum),                    &
     &          fln_tce%v_fline_start(1,icou))
            call s_sel_interpolate_scalar_ele                           &
     &         (1, node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,     &
     &          nod_fld%d_fld(1,fln_prm%iphys_4_fline+1),               &
     &          istack_tbl_wtype_smp(3), ione,                          &
     &          fln_src%iele_surf_start_fline(inum),                    &
     &          fln_src%xi_surf_start_fline(1,inum),                    &
     &          fln_tce%v_fline_start(2,icou))
            call s_sel_interpolate_scalar_ele                           &
     &         (1, node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,     &
     &          nod_fld%d_fld(1,fln_prm%iphys_4_fline+2),               &
     &          istack_tbl_wtype_smp(3), ione,                          &
     &          fln_src%iele_surf_start_fline(inum),                    &
     &          fln_src%xi_surf_start_fline(1,inum),                    &
     &          fln_tce%v_fline_start(3,icou))
          fln_tce%v_fline_start(4,icou) = one
!
          call cal_fields_in_element                                    &
     &       (fln_src%iele_surf_start_fline(inum),                      &
     &        fln_src%xi_surf_start_fline(1,inum),                      &
     &        fln_prm%xx_surf_start_fline(1,inum),                      &
     &        ele, nod_fld, fln_prm%fline_fields,                       &
     &        fln_tce%c_fline_start(1,icou))
!
          fln_tce%isf_dbl_start(1,icou) = my_rank
          fln_tce%isf_dbl_start(2,icou)                                 &
     &      = fln_src%iele_surf_start_fline(inum)
          fln_tce%isf_dbl_start(3,icou) = 0
          fln_tce%xx_fline_start(1:3,icou)                              &
     &         = fln_prm%xx_surf_start_fline(1:3,inum)
          fln_tce%xx_fline_start(4,icou) = one
          fln_tce%trace_length(icou) = 0.0d0
          fln_tce%icount_fline(icou) = 0
          
          if     (fln_prm%id_fline_direction                            &
     &                  .eq. iflag_forward_trace) then
           fln_tce%iflag_direction(icou) = 0
          else if(fln_prm%id_fline_direction                            &
     &                  .eq. iflag_backward_trace) then

            fln_tce%iflag_direction(icou) = 1
          else
            fln_tce%iflag_direction(icou) = 0
!
            icou = icou + 1
            fln_tce%iflag_direction(icou) = 1
            fln_tce%isf_dbl_start(1,icou) = my_rank
            fln_tce%isf_dbl_start(2,icou)                               &
     &            = fln_src%iele_surf_start_fline(inum)
            fln_tce%isf_dbl_start(3,icou) = 0
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
      end module set_fline_seeds_from_list
