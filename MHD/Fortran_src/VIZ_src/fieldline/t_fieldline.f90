!>@file   t_fieldline.f90
!!@brief  module t_fieldline
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
!!@endverbatim
!
      module t_fieldline
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_next_node_ele_4_node
      use t_control_params_4_fline
      use t_source_of_filed_line
      use t_local_fline
      use t_ucd_data
!
      implicit  none
!
      type fieldline_module
        integer(kind = kint) :: num_fline
!
        type(fieldline_paramter), allocatable :: fln_prm(:)
!
        type(each_fieldline_source), allocatable :: fln_src(:)
        type(each_fieldline_trace), allocatable :: fln_tce(:)
!
        type(local_fieldline) :: fline_lc
        type(ucd_data) :: fline_ucd
      end type fieldline_module
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine FLINE_initialize                                       &
     &         (increment_fline, fem, nod_fld, fline_ctls, fline)
!
      use calypso_mpi
      use calypso_mpi_int
      use m_connect_hexa_2_tetra
      use t_control_data_flines
      use t_find_interpolate_in_ele
      use set_fline_control
      use quicksort
!
      integer(kind = kint), intent(in) :: increment_fline
      type(mesh_data), intent(in) :: fem
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_controls), intent(inout) :: fline_ctls
      type(fieldline_module), intent(inout) :: fline
!
      type(cal_interpolate_coefs_work), save :: itp_ele_work_f
      integer(kind = kint), parameter :: maxitr = 20
      real(kind = kreal), parameter ::   eps_iter = 1.0d-9
      integer(kind = kint), parameter :: iflag_nomessage = 0
      real(kind = kreal), parameter ::   error_level = 0.0
      integer(kind = kint) :: ierr_inter
!
      real(kind = kreal), allocatable :: x(:)
      real(kind = kreal), allocatable :: y(:)
      real(kind = kreal), allocatable :: z(:)
      real(kind = kreal), allocatable :: ele_size(:)
      real(kind = kreal), allocatable :: size_max(:)

      integer(kind = kint), allocatable :: index(:)
      real(kind = kreal), allocatable :: distance(:)
      real(kind = kreal) :: dist_tmp
      real(kind = kreal) :: xi(3)
      integer(kind = kint) :: i_fln, i, iele, k1, inum, inod, ip, icou
      integer(kind = kint) :: num_search
!
      fline%num_fline = fline_ctls%num_fline_ctl
      if(increment_fline .le. 0) fline%num_fline = 0
      if(fline%num_fline .le. 0) return
!
      allocate(fline%fln_prm(fline%num_fline))
      allocate(fline%fln_src(fline%num_fline))
      allocate(fline%fln_tce(fline%num_fline))
!
      do i_fln = 1, fline%num_fline
        call s_set_fline_control(fem%mesh, fem%group, nod_fld,          &
     &      fline_ctls%fline_ctl_struct(i_fln), fline%fln_prm(i_fln),   &
     &      fline%fln_src(i_fln))
      end do
!
      call dealloc_fline_ctl_struct(fline_ctls)
!
      do i_fln = 1, fline%num_fline
        call alloc_local_data_4_fline                                   &
     &     (fem%mesh%node, fline%fln_src(i_fln))
        call alloc_start_point_fline                                    &
     &     (fline%fln_prm(i_fln), fline%fln_src(i_fln))
        call alloc_num_gl_start_fline(nprocs,                           &
     &      fline%fln_prm(i_fln), fline%fln_tce(i_fln))
      end do
!
      call alloc_local_fline(fline%fline_lc)
      allocate(x(fem%mesh%ele%nnod_4_ele))
      allocate(y(fem%mesh%ele%nnod_4_ele))
      allocate(z(fem%mesh%ele%nnod_4_ele))

      allocate(ele_size(fem%mesh%ele%numele))
      allocate(size_max(fem%mesh%ele%numele))
      allocate(distance(fem%mesh%ele%numele))
      allocate(index(fem%mesh%ele%numele))

      if (fem%mesh%ele%nnod_4_ele .eq. num_t_linear) then
        call set_1_hexa_2_5_tetra
      else if (fem%mesh%ele%nnod_4_ele .eq. num_t_quad) then
        call set_1_hexa_2_21_tetra
      else if (fem%mesh%ele%nnod_4_ele .eq. num_t_lag) then
        call set_1_hexa_2_40_tetra
      end if
!$omp parallel do private(iele,k1,inod,x,y,z)
      do iele = 1, fem%mesh%ele%numele
        do k1 = 1, fem%mesh%ele%nnod_4_ele
          inod = fem%mesh%ele%ie(iele,k1)
          x(k1) = fem%mesh%node%xx(inod,1)
          y(k1) = fem%mesh%node%xx(inod,2)
          z(k1) = fem%mesh%node%xx(inod,3)
        end do
        size_max(1) = maxval(x) - minval(x)
        size_max(2) = maxval(y) - minval(y)
        size_max(3) = maxval(z) - minval(z)
        size_max(iele) = sqrt(size_max(1)*size_max(1)                   &
     &                      + size_max(2)*size_max(2)                   &
     &                      + size_max(3)*size_max(3))
!        size_max(iele) = size_max(1) + size_max(2) + size_max(3)
      end do
!$omp end parallel do
      call calypso_mpi_barrier
      call alloc_work_4_interpolate(fem%mesh%ele%nnod_4_ele,            &
    &                               itp_ele_work_f)

      do i_fln = 1, fline%num_fline
        if(fline%fln_prm(i_fln)%id_fline_seed_type                      &
    &                       .eq. iflag_position_list) then
          do i = 1, fline%fln_prm(i_fln)%num_each_field_line
            x(1) = fline%fln_prm(i_fln)%xx_surf_start_fline(1,i)
            y(1) = fline%fln_prm(i_fln)%xx_surf_start_fline(2,i)
            z(1) = fline%fln_prm(i_fln)%xx_surf_start_fline(3,i)
            num_search = 0
            do iele = 1, fem%mesh%ele%numele
              dist_tmp = sqrt ((x(1) - fem%mesh%ele%x_ele(iele,1))**2   &
     &                       + (y(1) - fem%mesh%ele%x_ele(iele,2))**2   &
     &                       + (z(1) - fem%mesh%ele%x_ele(iele,3))**2)
              if(dist_tmp .le. size_max(iele)) then
                num_search = num_search + 1
                index(num_search) =    iele
                distance(num_search) = dist_tmp
              end if
            end do

            if(num_search .gt. 1) then
              call quicksort_real_w_index(fem%mesh%ele%numele,          &
    &             distance(1), ione, num_search, index(1))
            end if
!
            fline%fln_prm(i_fln)%ip_surf_start_fline(i) = -1
            fline%fln_prm(i_fln)%iele_surf_start_fline(i) = 0
            fline%fln_prm(i_fln)%xi_surf_start_fline(1:3,i) = -2.0
            do inum = 1, num_search
              iele = index(inum)
              if(fem%mesh%ele%ie(iele,1)                                & 
     &                .gt. fem%mesh%node%internal_node) cycle
              ierr_inter = 0
              xi(1:3) = -2.0
              call find_interpolate_in_ele                              &
     &           (fline%fln_prm(i_fln)%xx_surf_start_fline(1,i),        &
     &            maxitr, eps_iter,                                     &
     &            my_rank, iflag_nomessage, error_level,                &
     &            fem%mesh%node, fem%mesh%ele, iele,                    &
     &            itp_ele_work_f, xi, ierr_inter)
              if(ierr_inter.gt.1 .and. ierr_inter.le.maxitr) exit
            end do
            if(num_search .gt. 0 .and. ierr_inter.gt.1 .and. ierr_inter.le.maxitr) then
              fline%fln_prm(i_fln)%ip_surf_start_fline(i) = my_rank
              fline%fln_prm(i_fln)%iele_surf_start_fline(i) = iele
              fline%fln_prm(i_fln)%xi_surf_start_fline(1:3,i) = xi(1:3)
            end if
          end do
        end if
      end do

      do i_fln = 1, fline%num_fline
        do ip = 1, nprocs
        call calypso_mpi_barrier
        if(my_rank .ne. ip-1) cycle
          do i = 1, fline%fln_prm(i_fln)%num_each_field_line
            if(fline%fln_prm(i_fln)%ip_surf_start_fline(i) .ge. 0) then
            write(*,*) my_rank, i_fln, i, 'fline%fln_prm(i_fln)',    &
     &        fline%fln_prm(i_fln)%ip_surf_start_fline(i),           &
     &        fline%fln_prm(i_fln)%iele_surf_start_fline(i),         &
     &        fline%fln_prm(i_fln)%xi_surf_start_fline(1:3,i),       &
              fem%mesh%ele%numele, ierr_inter
            end if
          end do
        end do
!
        call calypso_mpi_barrier
        fline%fln_src(i_fln)%num_line_local = 0
        do i = 1, fline%fln_prm(i_fln)%num_each_field_line
        if(fline%fln_prm(i_fln)%ip_surf_start_fline(i) .eq. my_rank)   &
          fline%fln_src(i_fln)%num_line_local    &
     &      = fline%fln_src(i_fln)%num_line_local + 1
        end do
        call calypso_mpi_barrier
        write(*,*) my_rank, 'fline%fln_src(i_fln)%num_line_local', &
     &            fline%fln_src(i_fln)%num_line_local

        call calypso_mpi_allgather_one_int                              &
     &     (fline%fln_src(i_fln)%num_line_local, fline%fln_tce(i_fln)%num_current_fline)
        fline%fln_tce(i_fln)%istack_current_fline(0) = 0
        do i = 1, nprocs
          fline%fln_tce(i_fln)%istack_current_fline(i)   &
     &        = fline%fln_tce(i_fln)%istack_current_fline(i-1)   &
     &         + fline%fln_tce(i_fln)%num_current_fline(i)
        end do
      end do


      deallocate(size_max, ele_size, distance, index)
      call dealloc_work_4_interpolate(itp_ele_work_f)

!
      end subroutine FLINE_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine FLINE_visualize                                        &
     &         (istep_fline, time_d, fem, next_tbl, nod_fld, fline)
!
      use set_fields_for_fieldline
      use const_field_lines
      use collect_fline_data
      use parallel_ucd_IO_select
      use t_solver_ordered_crs
      use interpolate_matrix_para
      use interpolate_vector_1pe
      use interpolate_scalar_1pe
!
!
      integer(kind = kint), intent(in) :: istep_fline
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: fem
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(phys_data), intent(in) :: nod_fld
!
      type(fieldline_module), intent(inout) :: fline
!
      type(time_data) :: t_IO
      integer(kind = kint) :: i_fln, icou, inum
!
      type(CRS_SMP_CONNECT_MATRIX) :: mat
      integer(kind = kint) :: istack_tbl_wtype_smp(0:4)
      integer(kind = kint) :: itype_start_fline(1)
      real(kind = kreal) :: color_start(1)
      real(kind = kreal) :: vector_start(3)
      real(kind = kreal) :: position_check(3)
      real(kind = kreal) :: x4_start(4), v4_start(4)
!  
      if (fline%num_fline.le.0 .or. istep_fline.le.0) return
!
      if (iflag_debug.eq.1) write(*,*) 'set_local_field_4_fline'
      do i_fln = 1, fline%num_fline
        call set_local_field_4_fline(fem%mesh%node, nod_fld,            &
     &    fline%fln_prm(i_fln), fline%fln_src(i_fln))
!
        if(fline%fln_prm(i_fln)%id_fline_seed_type                      &
    &                       .eq. iflag_position_list) then

        call calypso_mpi_barrier
        write(*,*) my_rank, 'fline%fln_tce(i_fln)%istack_current_fline', &
     &            fline%fln_tce(i_fln)%istack_current_fline
!
        icou = 0
        do inum = 1, fline%fln_prm(i_fln)%num_each_field_line
          if(fline%fln_prm(i_fln)%ip_surf_start_fline(inum) .ne. my_rank) cycle
            icou = icou + 1


            mat%NC = 1
            mat%NUM_NCOMP = 4
            istack_tbl_wtype_smp(0:3) = 0
            istack_tbl_wtype_smp(4) =   1
            itype_start_fline(1) = 0
            call alloc_crs_smp_num(1, mat)
            call count_interporate_mat_para                         &
     &   (1, fem%mesh%ele%nnod_4_ele, istack_tbl_wtype_smp,  &
     &    mat%NC, mat%NUM_NCOMP, mat%NCM, mat%INOD_DJO,  mat%INM,   &
     &    mat%NUM_SUM, mat%IEND_SUM, mat%IEND_SUM_smp)
            call alloc_crs_smp_mat(mat)
          call set_interporate_mat_para                                 &
     &     (1, fem%mesh%ele%numele, fem%mesh%ele%nnod_4_ele,  &
     &    fem%mesh%ele%ie, &
     &    fline%fln_prm(i_fln)%iele_surf_start_fline(inum),  &
     &    itype_start_fline,         &
     &    fline%fln_prm(i_fln)%xi_surf_start_fline(1,inum), &
     &      mat%NC, mat%NCM, mat%INM,    &
     &    mat%IAM, mat%AM, mat%IEND_SUM_smp)

     
           call itp_matvec_vector     &
     &        (1, ione, fem%mesh%node%xx,              &
     &      mat%NC, mat%NCM, mat%INM, mat%IAM,      &
     &      mat%AM, mat%NUM_SUM(4), mat%IEND_SUM_smp,   &
     &      position_check)
           call itp_matvec_vector     &
     &        (1, ione, fline%fln_src(i_fln)%vector_nod_fline, &
     &      mat%NC, mat%NCM, mat%INM, mat%IAM,      &
     &      mat%AM, mat%NUM_SUM(4), mat%IEND_SUM_smp,   &
     &      vector_start)
          call itp_matvec_scalar   &
     &       (1, ione, fline%fln_src(i_fln)%color_nod_fline,  &
     &      mat%NC, mat%NCM, mat%INM, mat%IAM,          &
     &      mat%AM, mat%NUM_SUM(4), mat%IEND_SUM_smp,       &
     &      color_start)
           call dealloc_crs_smp_mat(mat)
!
          write(*,*) 'check', icou, position_check
          write(*,*) 'target', icou, fline%fln_prm(i_fln)%xx_surf_start_fline(1:3,inum)
!           
          x4_start(1:3) = fline%fln_prm(i_fln)%xx_surf_start_fline(1:3,inum)
          v4_start(1:3) = vector_start(1:3)
          fline%fln_tce(i_fln)%isf_fline_start(1,icou)   &
     &      = fline%fln_prm(i_fln)%iele_surf_start_fline(inum)
          fline%fln_tce(i_fln)%isf_fline_start(2,icou) = 0
          fline%fln_tce(i_fln)%xx_fline_start(1:4,icou) =  x4_start(1:4)
          fline%fln_tce(i_fln)%v_fline_start(1:4,icou) =   v4_start(1:4)
          fline%fln_tce(i_fln)%c_fline_start(icou) =       color_start(1)
          fline%fln_tce(i_fln)%icount_fline(icou) = 1
          fline%fln_tce(i_fln)%iflag_fline(icou) = 1
        end do
     



        else




        if (iflag_debug.eq.1) write(*,*) 's_set_fields_for_fieldline'
        call s_set_fields_for_fieldline(fem%mesh, fem%group,            &
     &      fline%fln_prm(i_fln), fline%fln_src(i_fln),                 &
     &      fline%fln_tce(i_fln))
        end if
      end do
!

      do i_fln = 1, fline%num_fline
        if (iflag_debug.eq.1) write(*,*) 's_const_field_lines', i_fln
        call s_const_field_lines(fem%mesh%node, fem%mesh%ele,           &
     &      fem%mesh%surf, next_tbl%neib_ele, fem%mesh%nod_comm,        &
     &      fline%fln_prm(i_fln), fline%fln_src(i_fln),                 &
     &      fline%fln_tce(i_fln), fline%fline_lc)
!
        call copy_time_step_size_data(time_d, t_IO)
        call copy_local_fieldline_to_IO                                 &
     &     (fline%fln_prm(i_fln)%name_color_output,                     &
     &      fline%fline_lc, fline%fline_ucd)
        call sel_write_parallel_ucd_file                                &
     &     (istep_fline, fline%fln_prm(i_fln)%fline_file_IO, t_IO,      &
     &      fline%fline_ucd)
        call deallocate_parallel_ucd_mesh(fline%fline_ucd)
        call calypso_mpi_barrier
      end do
!
      end subroutine FLINE_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine FLINE_finalize(fline)
!
      type(fieldline_module), intent(inout) :: fline
!
      integer(kind = kint) :: i
!
!
      if (fline%num_fline .le. 0) return
!
      call dealloc_local_fline(fline%fline_lc)
!
      do i = 1, fline%num_fline
        call dealloc_iflag_fline_used_ele(fline%fln_prm(i))
        call dealloc_fline_starts_ctl(fline%fln_prm(i))
!
        call dealloc_local_start_grp_item(fline%fln_src(i))
        call dealloc_local_data_4_fline(fline%fln_src(i))
        call dealloc_start_point_fline(fline%fln_src(i))
        call dealloc_num_gl_start_fline(fline%fln_tce(i))
      end do
!
      deallocate(fline%fln_src, fline%fln_tce, fline%fln_prm)
!
      end subroutine FLINE_finalize
!
!  ---------------------------------------------------------------------
!
      end module t_fieldline
