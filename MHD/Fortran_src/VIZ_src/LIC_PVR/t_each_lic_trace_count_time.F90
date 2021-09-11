!>@file   t_each_lic_trace_count_time.F90
!!@brief  module t_each_lic_trace_count_time
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!>@brief structure of control data for multiple LIC rendering
!!
!!@verbatim
!!      subroutine init_icou_int_nod_smp(node, l_elsp)
!!      subroutine dealloc_icou_int_nod_smp(l_elsp)
!!        type(node_data), intent(in) :: node
!!        type(each_lic_trace_counts), intent(inout) :: l_elsp
!!
!!      subroutine sum_icou_int_nod_smp                                 &
!!     &         (node, ele, l_elsp, count_int_nod)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(lic_parameters), intent(in) :: lic_p
!!        type(each_lic_trace_counts), intent(inout) :: l_elsp
!!        real(kind = kreal), intent(inout) :: count_int_nod(node%numnod)
!!      subroutine cal_trace_time_statistic(node, ele, lic_p, field_lic,&
!!     &          l_elsp, elapse_ray_trace_out)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(lic_parameters), intent(in) :: lic_p
!!        type(lic_field_data), intent(in) :: field_lic
!!        type(each_lic_trace_counts), intent(inout) :: l_elsp
!!        real(kind = kreal), intent(inout) :: elapse_ray_trace_out(2)
!!@endverbatim
!
      module t_each_lic_trace_count_time
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_geometry_data
      use t_lic_field_data
      use t_control_param_LIC
!
      implicit  none
!
      type lic_line_counter_smp
!         Conter for calling line integration routine
        integer(kind = kint) :: icount_lint_smp
!>        Elapsed time for line integration
        real(kind = kreal) :: elapse_lint_smp
!>        Number of internal node
        integer(kind = kint) :: nnod
!         Line integration count or rendering time for each internal node
        real(kind = kreal), allocatable :: rcount_int_nod(:)
      end type lic_line_counter_smp
!
!
      type each_lic_trace_counts
!>        Number of OpenMP threads
        integer :: np_smp_sys
!>        line integration counts for each node and OopenMP threads
        type(lic_line_counter_smp), allocatable :: line_count_smp(:)
!
!>        Total integration counts in each subdomain
        real(kind = kreal) :: count_line_intgrate
!
!>        Counter for ray tracing
        integer(kind = kint) :: icount_trace
!>        Elapsed time for ray tracing
        real(kind = kreal) :: elapse_rtrace
!>        Elapsed time for line integration
        real(kind = kreal) :: elapse_line_int
!
!>        Total integration counts in each subdomain
        real(kind = kreal) :: count_line_int_gl
!>        Elapsed time for ray tracing
        real(kind = kreal) :: elapse_rtrace_gl
!>        Elapsed time for line integration
        real(kind = kreal) :: elapse_line_int_gl
      end type each_lic_trace_counts
!
      type(each_lic_trace_counts) :: l_elsp
!
      private :: nnod_masked_4_LIC
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_rcount_int_nod(internal_node, line_count_smp)
!
      integer(kind = kint), intent(in) :: internal_node
      type(lic_line_counter_smp), intent(inout) :: line_count_smp
!
      line_count_smp%nnod = internal_node
      allocate(line_count_smp%rcount_int_nod(line_count_smp%nnod))
!$omp parallel workshare
      line_count_smp%rcount_int_nod(1:line_count_smp%nnod) = zero
!$omp end parallel workshare
!
      end subroutine alloc_rcount_int_nod
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_rcount_int_nod(line_count_smp)
!
      type(lic_line_counter_smp), intent(inout) :: line_count_smp
!
      deallocate(line_count_smp%rcount_int_nod)
!
      end subroutine dealloc_rcount_int_nod
!
!  ---------------------------------------------------------------------
!
      subroutine init_icou_int_nod_smp(node, l_elsp)
!
      type(node_data), intent(in) :: node
      type(each_lic_trace_counts), intent(inout) :: l_elsp
!
      integer(kind = kint) :: ip
!
#ifdef _OPENMP
      integer, external :: omp_get_max_threads
#endif
!
      l_elsp%np_smp_sys = 1
#ifdef _OPENMP
      l_elsp%np_smp_sys = omp_get_max_threads()
#endif
!
      allocate(l_elsp%line_count_smp(l_elsp%np_smp_sys))
!      if(lic_p%each_part_p%iflag_repart_ref                            &
!     &                     .eq. i_INT_COUNT_BASED) then
      do ip = 1, l_elsp%np_smp_sys
        call alloc_rcount_int_nod(node%internal_node,                   &
     &                            l_elsp%line_count_smp(ip))
      end do
!      end if
!
      end subroutine init_icou_int_nod_smp
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_icou_int_nod_smp(l_elsp)
!
      type(each_lic_trace_counts), intent(inout) :: l_elsp
      integer(kind = kint) :: ip
!
!
!      if(lic_p%each_part_p%iflag_repart_ref                            &
!     &                     .eq. i_INT_COUNT_BASED) then
      do ip = 1, l_elsp%np_smp_sys
        call dealloc_rcount_int_nod(l_elsp%line_count_smp(ip))
      end do
!      end if
      deallocate(l_elsp%line_count_smp)
!
      end subroutine dealloc_icou_int_nod_smp
!
!  ---------------------------------------------------------------------
!
      subroutine sum_icou_int_nod_smp                                   &
     &         (node, ele, l_elsp, count_int_nod)
!
      use calypso_mpi_real
!      use int_volume_of_single_domain
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(each_lic_trace_counts), intent(inout) :: l_elsp
      real(kind = kreal), intent(inout) :: count_int_nod(node%numnod)
!
      integer(kind = kint) :: ip, inod
      real(kind = kreal) :: count_line_tmp
!      real(kind = kreal) :: anorm_rtrace_gl, elapse_lic_gl
!      real(kind = kreal) :: anorm_line_int_gl
!      real(kind = kreal), allocatable :: volume_nod_cnt(:)
!
!
      l_elsp%icount_trace = l_elsp%line_count_smp(1)%icount_lint_smp
      do ip = 2, l_elsp%np_smp_sys
        l_elsp%icount_trace = l_elsp%icount_trace                       &
     &     + l_elsp%line_count_smp(ip)%icount_lint_smp
        l_elsp%elapse_line_int = l_elsp%elapse_line_int                 &
     &     + l_elsp%line_count_smp(ip)%elapse_lint_smp
      end do
!
      l_elsp%elapse_line_int = l_elsp%elapse_line_int                   &
     &                        / dble(l_elsp%np_smp_sys)
!
!$omp workshare
      count_int_nod(1:node%numnod) =  0.0d0
!$omp end workshare
!
!      if(lic_p%each_part_p%iflag_repart_ref                            &
!     &                     .eq. i_INT_COUNT_BASED) then
!$omp parallel
          do ip = 1, l_elsp%np_smp_sys
!$omp do
            do inod = 1, node%internal_node
              count_int_nod(inod) =  count_int_nod(inod)                &
     &             + l_elsp%line_count_smp(ip)%rcount_int_nod(inod)
            end do
!$omp end do nowait
          end do
!$omp end parallel
!      end if
!
!$omp workshare
      count_int_nod(1:node%numnod) = count_int_nod(1:node%numnod)       &
     &                              / dble(ele%nnod_4_ele)
!$omp end workshare
!
      count_line_tmp = 0
!$omp parallel do reduction(+:count_line_tmp)
      do inod = 1, node%internal_node
        count_line_tmp = count_line_tmp + count_int_nod(inod)
      end do
!$omp end parallel do
      l_elsp%count_line_intgrate = count_line_tmp
!
!
      call calypso_mpi_allreduce_one_real                               &
     &   (l_elsp%count_line_intgrate, l_elsp%count_line_int_gl,         &
     &    MPI_SUM)
      call calypso_mpi_allreduce_one_real                               &
     &   (l_elsp%elapse_rtrace, l_elsp%elapse_rtrace_gl, MPI_SUM)
      call calypso_mpi_allreduce_one_real                               &
     &   (l_elsp%elapse_line_int, l_elsp%elapse_line_int_gl, MPI_SUM)
!
!      elapse_lic_gl = l_elsp%elapse_rtrace_gl                          &
!     &               + l_elsp%elapse_line_int_gl
!      anorm_line_int_gl = l_elsp%elapse_line_int_gl                    &
!     &                   / (l_elsp%count_line_int_gl * elapse_lic_gl)
!      anorm_rtrace_gl =  l_elsp%elapse_rtrace_gl                       &
!     &                  / (ele%volume * elapse_lic_gl)
!
!      allocate(volume_nod_cnt(node%numnod))
!!$omp workshare
!      volume_nod_cnt(1:node%numnod) = 0.0d0
!!$omp end workshare
!      call cal_node_volue(node, ele, volume_nod_cnt)
!!$omp workshare
!      count_int_nod(1:node%numnod)                                     &
!     &    = count_int_nod(1:node%numnod) * anorm_line_int_gl           &
!     &     + volume_nod_cnt(1:node%numnod) * anorm_rtrace_gl
!!$omp end workshare
!      deallocate(volume_nod_cnt)
!
      end subroutine sum_icou_int_nod_smp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_trace_time_statistic(node, ele, lic_p, field_lic,  &
     &          l_elsp, elapse_ray_trace_out)
!
      use calypso_mpi_int
      use calypso_mpi_real
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(lic_parameters), intent(in) :: lic_p
      type(lic_field_data), intent(in) :: field_lic
!
      type(each_lic_trace_counts), intent(inout) :: l_elsp
      real(kind = kreal), intent(inout) :: elapse_ray_trace_out(2)
!
      integer(kind = kint) :: inum
!
!      type(noise_mask), allocatable :: n_mask
      integer(kind = kint) :: min_sample_cnt, max_sample_cnt
      real(kind = kreal) :: ave_sample_cnt, std_sample_cnt
!
      integer(kind = kint) :: min_line_int_cnt, max_line_int_cnt
      real(kind = kreal) :: ave_line_int_cnt, std_line_int_cnt
!
      real(kind = kreal) :: ave_trace_time,   std_trace_time
      real(kind = kreal) :: dmin_trace_time,  dmax_trace_time
      real(kind = kreal) :: ave_line_int_time, std_line_int_time
      real(kind = kreal) :: dmin_line_int_time, dmax_line_int_time
!
      real(kind = kreal) :: sq_sample_cnt, sq_line_int_cnt
      real(kind = kreal) :: sq_trace_time, sq_line_int_time
!
      integer(kind = kint), allocatable :: internal_node_out(:)
      integer(kind = kint), allocatable :: nnod_masked_out(:)
      integer(kind = kint), allocatable :: sample_cnt_out(:)
      integer(kind = kint), allocatable :: icou_line_int_out(:)
      real(kind = kreal), allocatable :: volume_out(:)
      real(kind = kreal), allocatable :: elapse_rtrace_out(:)
      real(kind = kreal), allocatable :: elapse_line_out(:)
!
!
      elapse_ray_trace_out(1)                                           &
     &     = l_elsp%elapse_rtrace / dble(node%internal_node)
      elapse_ray_trace_out(2) = l_elsp%elapse_line_int                  &
     &     / dble(nnod_masked_4_LIC(node, lic_p, field_lic))
      if(lic_p%flag_LIC_elapsed_dump) return
!
!      if(i_debug .gt. 0) write(*,*)                                    &
!      write(*,*) "pvr sampling cnt:", my_rank, l_elsp%icount_trace
!
      call calypso_mpi_allreduce_one_int                                &
     &   (l_elsp%icount_trace, min_sample_cnt, MPI_SUM)
      ave_sample_cnt =   dble(min_sample_cnt)
!
      call calypso_mpi_allreduce_one_real                               &
     &   (l_elsp%elapse_rtrace, dmin_trace_time, MPI_MIN)
      call calypso_mpi_allreduce_one_real                               &
     &   (l_elsp%elapse_line_int, dmin_line_int_time, MPI_MIN)
      call calypso_mpi_allreduce_one_int                                &
     &   (l_elsp%icount_trace, min_sample_cnt, MPI_MIN)
      call calypso_mpi_allreduce_one_int                                &
     &   (int(l_elsp%count_line_intgrate), min_line_int_cnt, MPI_MIN)
!
      call calypso_mpi_allreduce_one_real                               &
     &   (l_elsp%elapse_rtrace, dmax_trace_time, MPI_MAX)
      call calypso_mpi_allreduce_one_real                               &
     &   (l_elsp%elapse_line_int, dmax_line_int_time, MPI_MAX)
      call calypso_mpi_allreduce_one_int                                &
     &   (l_elsp%icount_trace, max_sample_cnt, MPI_MAX)
      call calypso_mpi_allreduce_one_int                                &
     &   (int(l_elsp%count_line_intgrate), max_line_int_cnt, MPI_MAX)
!
      ave_trace_time =    l_elsp%elapse_rtrace_gl / dble(nprocs)
      ave_line_int_time = l_elsp%elapse_line_int_gl / dble(nprocs)
      ave_line_int_cnt =  l_elsp%count_line_int_gl / dble(nprocs)
      ave_sample_cnt =    ave_sample_cnt / dble(nprocs)
!
      sq_trace_time =    (l_elsp%elapse_rtrace - ave_trace_time)**2
      sq_line_int_time = (l_elsp%elapse_line_int                        &
     &                  - ave_line_int_time)**2
      sq_sample_cnt =  (dble(l_elsp%icount_trace) - ave_sample_cnt)**2
      sq_line_int_cnt                                                   &
     &     = (l_elsp%count_line_intgrate - ave_line_int_cnt)**2
!
      call calypso_mpi_allreduce_one_real                               &
     &   (sq_trace_time,    std_trace_time, MPI_SUM)
      call calypso_mpi_allreduce_one_real                               &
     &   (sq_line_int_time, std_line_int_time, MPI_SUM)
      call calypso_mpi_allreduce_one_real                               &
     &   (sq_sample_cnt,    std_sample_cnt, MPI_SUM)
      call calypso_mpi_allreduce_one_real                               &
     &   (sq_line_int_cnt,  std_line_int_cnt, MPI_SUM)
!
      std_trace_time =    sqrt(std_trace_time / dble(nprocs))
      std_line_int_time = sqrt(std_line_int_time / dble(nprocs))
      std_sample_cnt =    sqrt(std_sample_cnt / dble(nprocs))
      std_line_int_cnt =  sqrt(std_line_int_cnt / dble(nprocs))
!
      if(my_rank .eq. 0) then
        write(*,*) 'Trace counts, rendering, line_integration'
        write(*,'(a,1p4e15.7)') 'Average:   ',                          &
     &          ave_sample_cnt, ave_line_int_cnt,                       &
     &          ave_trace_time, ave_line_int_time
        write(*,'(a,1p4e15.7)') 'Deviation: ',                          &
     &          std_sample_cnt, std_line_int_cnt,                       &
     &          std_trace_time, std_line_int_time
        write(*,'(a,2i15,1p2e15.7)') 'Minimum:   ',                     &
     &          min_sample_cnt, min_line_int_cnt,                       &
     &          dmin_trace_time, dmin_line_int_time
        write(*,'(a,2i15,1p2e15.7)') 'Maximum:   ',                     &
     &          max_sample_cnt, max_line_int_cnt,                       &
     &          dmax_trace_time, dmax_line_int_time
      end if
!
      if(my_rank .eq. 0) then
        allocate(internal_node_out(nprocs))
        allocate(nnod_masked_out(nprocs))
        allocate(sample_cnt_out(nprocs))
        allocate(icou_line_int_out(nprocs))
        allocate(volume_out(nprocs))
        allocate(elapse_rtrace_out(nprocs))
        allocate(elapse_line_out(nprocs))
      end if
!
      call calypso_mpi_gather_one_int                                   &
     &   (nnod_masked_4_LIC(node, lic_p, field_lic), nnod_masked_out,0)
      call calypso_mpi_gather_one_int                                   &
     &   (node%internal_node, internal_node_out, 0)
      call calypso_mpi_gather_one_int                                   &
     &   (l_elsp%icount_trace, sample_cnt_out, 0)
      call calypso_mpi_gather_one_int                                   &
     &   (int(l_elsp%count_line_intgrate), icou_line_int_out, 0)
      call calypso_mpi_gather_one_real                                  &
     &   (l_elsp%elapse_rtrace, elapse_rtrace_out, 0)
      call calypso_mpi_gather_one_real                                  &
     &   (l_elsp%elapse_line_int, elapse_line_out, 0)
      call calypso_mpi_gather_one_real                                  &
     &   (ele%volume_local, volume_out, 0)
!
      if(my_rank .eq. 0) then
        open(999,file='LIC_elapsed.dat', position='APPEND')
        write(999,'(3a)')  'rank, internal_nod, nod_masked, volume, ',  &
     &                  ' ray_trace_count, line_integration_count, ',   &
     &                  ' ray_trace_time, line_integration_time'
        do inum = 1, nprocs
          write(999,'(3i15,1pe15.7,2i15,1p2e15.7)')                     &
     &       (inum-1), internal_node_out(inum),                         &
     &       nnod_masked_out(inum), volume_out(inum),                   &
     &       sample_cnt_out(inum), icou_line_int_out(inum),             &
     &       elapse_rtrace_out(inum), elapse_line_out(inum)
        end do
        close(999)
!
        deallocate(internal_node_out)
        deallocate(nnod_masked_out)
        deallocate(icou_line_int_out)
        deallocate(sample_cnt_out)
        deallocate(volume_out)
        deallocate(elapse_rtrace_out)
        deallocate(elapse_line_out)
      end if
!
      end subroutine cal_trace_time_statistic
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function nnod_masked_4_LIC                   &
     &                            (node, lic_p, field_lic)
!
      type(node_data), intent(in) :: node
      type(lic_parameters), intent(in) :: lic_p
      type(lic_field_data), intent(in) :: field_lic
!
      real(kind = kreal), allocatable :: value(:,:)
      integer(kind = kint), allocatable :: icou_smp(:)
      integer(kind = kint) :: ip, ist, ied, inod
!
!
      allocate(value(lic_p%num_masking,np_smp))
      allocate(icou_smp(np_smp))
!
      icou_smp(1:np_smp) = 0
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = node%istack_internal_smp(ip-1) + 1
        ied = node%istack_internal_smp(ip)
        do inod = ist, ied
          value(1:lic_p%num_masking,ip)                                 &
     &          = field_lic%s_lic(inod,1:lic_p%num_masking)
          if(lic_mask_flag(lic_p, value(1,ip))) then
            icou_smp(ip) = icou_smp(ip) + 1
          end if
        end do
      end do
!$omp end parallel do
!
      nnod_masked_4_LIC = 1 + sum(icou_smp)
      deallocate(value, icou_smp)
!
      end function nnod_masked_4_LIC
!
! ----------------------------------------------------------------------
!
      end module t_each_lic_trace_count_time
