!>@file   collect_fline_data.f90
!!@brief  module collect_fline_data
!!
!!@author  H. Matsui
!!@date Programmed on Aug., 2011
!
!> @brief MPI communication To collect field line data
!!
!!@verbatim
!!      subroutine copy_local_fieldline_to_IO(ele, nod_fld, viz_fields, &
!!     &                                      fline_lc, ucd)
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        type(local_fieldline), intent(in) :: fline_lc
!!        type(ucd_data), intent(inout) :: ucd
!!      subroutine copy_local_particles_to_IO(ele, nod_fld, viz_fields, &
!!     &                                      fln_tce, ucd)
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!
      module collect_fline_data
!
      use m_precision
      use m_machine_parameter
!
      use calypso_mpi
      use m_constants
      use m_geometry_constants
      use t_geometry_data
      use t_phys_data
      use t_ucd_data
      use t_control_params_4_fline
      use t_tracing_data
      use t_local_fline
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_local_fieldline_to_IO(ele, nod_fld, viz_fields,   &
     &                                      fline_lc, ucd)
!
      use const_global_element_ids
      use tracer_field_interpolate
!
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      type(local_fieldline), intent(in) :: fline_lc
!
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint_gl) :: i, nd
      integer(kind = kint) :: j
      real(kind = kreal), allocatable :: c_ref(:)
!
!
      ucd%nnod = fline_lc%nnod_line_l
      ucd%nele = fline_lc%nele_line_l
      ucd%nnod_4_ele = num_linear_edge
!
      call alloc_merged_ucd_nod_stack(nprocs, ucd)
      call alloc_merged_ucd_ele_stack(nprocs, ucd)
      call count_number_of_node_stack(fline_lc%nnod_line_l,             &
     &                                ucd%istack_merged_nod)
      call count_number_of_node_stack(fline_lc%nele_line_l,             &
     &                                ucd%istack_merged_ele)
!      write(*,*) 'ucd%istack_merged_nod', ucd%istack_merged_nod
!      write(*,*) 'ucd%istack_merged_ele', ucd%istack_merged_ele
!
!$omp parallel workshare
      ucd%istack_merged_intnod(0:nprocs)                                &
     &                  = ucd%istack_merged_nod(0:nprocs)
!$omp end parallel workshare
!
      call allocate_ucd_node(ucd)
!$omp parallel do
      do i = 1, ucd%nnod
        ucd%inod_global(i) = fline_lc%iglobal_fline(i)
        ucd%xx(i,1) = fline_lc%xx_line_l(1,i)
        ucd%xx(i,2) = fline_lc%xx_line_l(2,i)
        ucd%xx(i,3) = fline_lc%xx_line_l(3,i)
      end do
!$omp end parallel do

      call allocate_ucd_ele(ucd)
!$omp parallel do
      do i = 1, ucd%nele
        ucd%iele_global(i) = i + ucd%istack_merged_ele(my_rank)
        ucd%ie(i,1) = fline_lc%iedge_line_l(1,i)                        &
     &               + ucd%istack_merged_nod(my_rank)
        ucd%ie(i,2) = fline_lc%iedge_line_l(2,i)                        &
     &               + ucd%istack_merged_nod(my_rank)
      end do
!$omp end parallel do
      
      ucd%num_field = viz_fields%num_color_fields
      call allocate_ucd_phys_name(ucd)
!$omp parallel workshare
      ucd%phys_name(1:ucd%num_field)                                    &
     &     = viz_fields%color_field_name(1:ucd%num_field)
      ucd%num_comp(1:ucd%num_field)                                     &
     &     = viz_fields%ncomp_color_field(1:ucd%num_field)
!$omp end parallel workshare
!
      ucd%ntot_comp = viz_fields%ntot_color_comp
      call allocate_ucd_phys_data(ucd)
!
      allocate(c_ref(viz_fields%ntot_color_comp))
!$omp parallel do private(c_ref)
      do i = 1, ucd%nnod
        call cal_fields_in_element(fline_lc%iele_fline(i),            &
     &     fline_lc%xi_line_l(1,i), fline_lc%xx_line_l(1,i),          &
     &     ele, nod_fld, viz_fields, c_ref(1))
!
        ucd%d_ucd(i,1:ucd%ntot_comp) = c_ref(1:ucd%ntot_comp)
      end do
!$omp end parallel do
      deallocate(c_ref)
!
      end subroutine copy_local_fieldline_to_IO
!
!  ---------------------------------------------------------------------
!
      subroutine copy_local_particles_to_IO(ele, nod_fld, viz_fields,   &
     &                                      fln_tce, ucd)
!
      use t_source_of_filed_line
      use const_global_element_ids
      use tracer_field_interpolate
!
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(ctl_params_viz_fields), intent(in) :: viz_fields
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint_gl) :: i, ip, ist, num
!
!
      ucd%nnod = fln_tce%num_current_fline
      ucd%nele = ucd%nnod
      ucd%nnod_4_ele = num_linear_point
!
      call alloc_merged_ucd_nod_stack(nprocs, ucd)
      call alloc_merged_ucd_ele_stack(nprocs, ucd)
      ucd%istack_merged_nod(0:nprocs)                                   &
     &    = fln_tce%istack_current_fline(0:nprocs) 
!
!$omp parallel workshare
      ucd%istack_merged_ele(0:nprocs)                                   &
     &                  = ucd%istack_merged_nod(0:nprocs)
      ucd%istack_merged_intnod(0:nprocs)                                &
     &                  = ucd%istack_merged_nod(0:nprocs)
!$omp end parallel workshare
!
      call allocate_ucd_node(ucd)
!$omp parallel do
      do i = 1, ucd%nnod
        ucd%inod_global(i) = fln_tce%iline_original(i)
        ucd%xx(i,1) =        fln_tce%xx_fline_start(1,i)
        ucd%xx(i,2) =        fln_tce%xx_fline_start(2,i)
        ucd%xx(i,3) =        fln_tce%xx_fline_start(3,i)
      end do
!$omp end parallel do

      call allocate_ucd_ele(ucd)
!$omp parallel do
      do i = 1, ucd%nele
        ucd%iele_global(i) = ucd%inod_global(i)
        ucd%ie(i,1) =        fln_tce%iline_original(i)
      end do
!$omp end parallel do
      
      ucd%num_field = viz_fields%num_color_fields
      call allocate_ucd_phys_name(ucd)
!$omp parallel workshare
      ucd%phys_name(1:ucd%num_field)                                    &
     &     = viz_fields%color_field_name(1:ucd%num_field)
      ucd%num_comp(1:ucd%num_field)                                     &
     &     = viz_fields%ncomp_color_field(1:ucd%num_field)
!$omp end parallel workshare

      ucd%ntot_comp = viz_fields%ntot_color_comp
      call allocate_ucd_phys_data(ucd)
!
!$omp parallel do private(ip,ist,num,i)
      do ip = 1, np_smp
        ist = fln_tce%istack_smp_cur_fline(ip-1)
        num = fln_tce%istack_smp_cur_fline(ip) - ist
        do i = 1, num
          call cal_fields_in_element(fln_tce%isf_dbl_start(2,i),        &
     &        fln_tce%xi_fline_start(1,i), fln_tce%xx_fline_start(1,i), &
     &        ele, nod_fld, viz_fields, fln_tce%c_fline_start(1,ip))
          ucd%d_ucd(i+ist,1:ucd%ntot_comp)                              &
     &       = fln_tce%c_fline_start(1:ucd%ntot_comp,ip)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_local_particles_to_IO
!
!  ---------------------------------------------------------------------
!
      end module collect_fline_data
