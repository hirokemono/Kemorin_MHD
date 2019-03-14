!t_work_sph_dynamic_grping.f90
!
!      module t_work_sph_dynamic_grping
!
!      Written by H. Matsui on Aug., 2018
!
!!      subroutine alloc_mk_sph_dgrp_flag(num_pe, wk_dgrp)
!!      subroutine alloc_mk_sph_dgrp_stack(wk_dgrp)
!!      subroutine alloc_mk_sph_istack_dynamic(SGS_param, wk_dgrp)
!!      subroutine dealloc_mk_sph_dgrp_flag(wk_dgrp)
!!      subroutine dealloc_mk_sph_istack_dynamic(wk_dgrp)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(make_sph_dynamic_model_grp), intent(inout) :: wk_dgrp
!!
!!      subroutine find_num_rt_grid_4_fluid(wk_dgrp)
!!      subroutine set_istack_dynamic_sph_grp(wk_dgrp)
!!        type(make_sph_dynamic_model_grp), intent(inout) :: wk_dgrp
!!
!!      subroutine ckeck_global_nums_4_dynamic(wk_dgrp)
!!      subroutine ckeck_make_dynamic_grp_stacks(wk_dgrp)
!!      subroutine check_num_grouping_sph_dynamic(nlayer_fluid,         &
!!     &          SGS_param, sph_rtp, wk_dgrp)
!!        type(make_sph_dynamic_model_grp), intent(in) :: wk_dgrp
!
      module t_work_sph_dynamic_grping
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
!
      type make_sph_dynamic_model_grp
        integer(kind = kint) :: nprocs_rt(2)
!
        integer(kind = kint), allocatable :: irank_list_r(:)
        integer(kind = kint), allocatable :: irank_list_t(:)
        integer(kind = kint), allocatable :: nri_pe_list(:)
        integer(kind = kint), allocatable :: nth_pe_list(:)
        integer(kind = kint), allocatable :: nri_1d_list(:)
        integer(kind = kint), allocatable :: nth_1d_list(:)
!
        integer(kind = kint), allocatable :: istack_global_kr(:)
        integer(kind = kint), allocatable :: istack_global_lt(:)
!
        integer(kind = kint), allocatable :: istack_r_gl_ngrp(:)
        integer(kind = kint), allocatable :: istack_t_gl_ngrp(:)
!
        integer(kind = kint), allocatable :: istack_rgrp(:)
        integer(kind = kint), allocatable :: istack_tgrp(:)
      end type make_sph_dynamic_model_grp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_mk_sph_dgrp_flag(num_pe, wk_dgrp)
!
      use t_spheric_rtp_data
!
      integer, intent(in) :: num_pe
      type(make_sph_dynamic_model_grp), intent(inout) :: wk_dgrp
!
!
      allocate(wk_dgrp%irank_list_r(num_pe))
      allocate(wk_dgrp%irank_list_t(num_pe))
      allocate(wk_dgrp%nri_pe_list(num_pe))
      allocate(wk_dgrp%nth_pe_list(num_pe))
      wk_dgrp%irank_list_r = -1
      wk_dgrp%irank_list_t = -1
      wk_dgrp%nri_pe_list = 0
      wk_dgrp%nth_pe_list = 0
!
      end subroutine alloc_mk_sph_dgrp_flag
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_mk_sph_dgrp_stack(wk_dgrp)
!
      type(make_sph_dynamic_model_grp), intent(inout) :: wk_dgrp
!
!
      allocate(wk_dgrp%nri_1d_list(wk_dgrp%nprocs_rt(1)))
      allocate(wk_dgrp%nth_1d_list(wk_dgrp%nprocs_rt(2)))
      wk_dgrp%nri_1d_list = 0
      wk_dgrp%nth_1d_list = 0
!
      allocate(wk_dgrp%istack_global_kr(0:wk_dgrp%nprocs_rt(1)))
      allocate(wk_dgrp%istack_global_lt(0:wk_dgrp%nprocs_rt(2)))
      allocate(wk_dgrp%istack_r_gl_ngrp(0:wk_dgrp%nprocs_rt(1)))
      allocate(wk_dgrp%istack_t_gl_ngrp(0:wk_dgrp%nprocs_rt(2)))
      wk_dgrp%istack_global_kr = 0
      wk_dgrp%istack_global_lt = 0
      wk_dgrp%istack_r_gl_ngrp = 0
      wk_dgrp%istack_t_gl_ngrp = 0
!
      end subroutine alloc_mk_sph_dgrp_stack
!
! -----------------------------------------------------------------------
!
      subroutine alloc_mk_sph_istack_dynamic(SGS_param, wk_dgrp)
!
      use t_SGS_control_parameter
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(make_sph_dynamic_model_grp), intent(inout) :: wk_dgrp
!
!
      allocate(wk_dgrp%istack_rgrp(0:SGS_param%ngrp_rave_dynamic))
      allocate(wk_dgrp%istack_tgrp(0:SGS_param%ngrp_medave_dynamic))
      wk_dgrp%istack_rgrp = 0
      wk_dgrp%istack_tgrp = 0
!
      end subroutine alloc_mk_sph_istack_dynamic
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_mk_sph_dgrp_flag(wk_dgrp)
!
      type(make_sph_dynamic_model_grp), intent(inout) :: wk_dgrp
!
!
      deallocate(wk_dgrp%irank_list_r, wk_dgrp%irank_list_t)
      deallocate(wk_dgrp%nri_pe_list, wk_dgrp%nth_pe_list)
!
      end subroutine dealloc_mk_sph_dgrp_flag
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_mk_sph_istack_dynamic(wk_dgrp)
!
      type(make_sph_dynamic_model_grp), intent(inout) :: wk_dgrp
!
!
      deallocate(wk_dgrp%nri_1d_list, wk_dgrp%nth_1d_list)
      deallocate(wk_dgrp%istack_global_kr, wk_dgrp%istack_global_lt)
      deallocate(wk_dgrp%istack_r_gl_ngrp, wk_dgrp%istack_t_gl_ngrp)
!
      deallocate(wk_dgrp%istack_rgrp)
      deallocate(wk_dgrp%istack_tgrp)
!
      end subroutine dealloc_mk_sph_istack_dynamic
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine find_num_rt_grid_4_fluid(wk_dgrp)
!
      type(make_sph_dynamic_model_grp), intent(inout) :: wk_dgrp
!
      integer(kind = kint) :: ist, igrp, ip
!
!
      ist = 1
      do igrp = 1, wk_dgrp%nprocs_rt(1)
        do ip = ist, nprocs
          if(wk_dgrp%irank_list_r(ip)+1 .eq. igrp) then
            wk_dgrp%nri_1d_list(igrp) = wk_dgrp%nri_pe_list(ip)
            ist = ip + 1
            exit
          end if
        end do
      end do
!
      ist = 1
      do igrp = 1, wk_dgrp%nprocs_rt(2)
        do ip = ist, nprocs
          if(wk_dgrp%irank_list_t(ip)+1 .eq. igrp) then
            wk_dgrp%nth_1d_list(igrp) = wk_dgrp%nth_pe_list(ip)
            ist = ip + 1
            exit
          end if
        end do
      end do
!
      end subroutine find_num_rt_grid_4_fluid
!
! -----------------------------------------------------------------------
!
      subroutine set_istack_dynamic_sph_grp(wk_dgrp)
!
      use cal_minmax_and_stacks
!
      type(make_sph_dynamic_model_grp), intent(inout) :: wk_dgrp
!
      integer(kind = kint) :: i, max, num, ist
      integer(kind = kint) :: kst, ked, lst, led
!
!
      do i = 1, wk_dgrp%nprocs_rt(1)
        num = wk_dgrp%istack_r_gl_ngrp(i)                               &
     &       - wk_dgrp%istack_r_gl_ngrp(i-1)
        ist = wk_dgrp%istack_r_gl_ngrp(i-1)
        kst = wk_dgrp%istack_global_kr(i-1) + 1
        ked = wk_dgrp%istack_global_kr(i)
        call count_number_4_smp(num, kst, ked,                          &
     &      wk_dgrp%istack_rgrp(ist), max)
      end do
!
      do i = 1, wk_dgrp%nprocs_rt(2)
        num = wk_dgrp%istack_t_gl_ngrp(i)                               &
     &       - wk_dgrp%istack_t_gl_ngrp(i-1)
        ist = wk_dgrp%istack_t_gl_ngrp(i-1)
        lst = wk_dgrp%istack_global_lt(i-1) + 1
        led = wk_dgrp%istack_global_lt(i)
        call count_number_4_smp(num, lst, led,                          &
     &      wk_dgrp%istack_tgrp(ist), max)
      end do
!
      end subroutine set_istack_dynamic_sph_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine ckeck_global_nums_4_dynamic(wk_dgrp)
!
      type(make_sph_dynamic_model_grp), intent(in) :: wk_dgrp
!
!
      write(*,*) 'irank_list_r', wk_dgrp%irank_list_r
      write(*,*) 'irank_list_t', wk_dgrp%irank_list_t
      write(*,*) 'nri_pe_list', wk_dgrp%nri_pe_list
      write(*,*) 'nth_pe_list', wk_dgrp%nth_pe_list
      write(*,*) 'nri_1d_list', wk_dgrp%nri_1d_list
!
      end subroutine ckeck_global_nums_4_dynamic
!
! -----------------------------------------------------------------------
!
      subroutine ckeck_make_dynamic_grp_stacks(wk_dgrp)
!
      type(make_sph_dynamic_model_grp), intent(in) :: wk_dgrp
!
!
      write(*,*) 'istack_global_kr', wk_dgrp%istack_global_kr
      write(*,*) 'istack_r_gl_ngrp', wk_dgrp%istack_r_gl_ngrp
      write(*,*) 'istack_rgrp',      wk_dgrp%istack_rgrp
      write(*,*) 'istack_global_lt', wk_dgrp%istack_global_lt
      write(*,*) 'istack_t_gl_ngrp', wk_dgrp%istack_t_gl_ngrp
      write(*,*) 'istack_tgrp',      wk_dgrp%istack_tgrp
!
      end subroutine ckeck_make_dynamic_grp_stacks
!
! -----------------------------------------------------------------------
!
      subroutine check_num_grouping_sph_dynamic(nlayer_fluid,           &
     &          SGS_param, sph_rtp, wk_dgrp)
!
      use t_SGS_control_parameter
      use t_spheric_parameter
      use t_spheric_rtp_data
      use m_error_IDs
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: nlayer_fluid
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      type(make_sph_dynamic_model_grp), intent(in) :: wk_dgrp
!
      integer(kind = kint) :: ip_r, ip_t, ngrp_r, ngrp_t
!
!
      ip_r =  sph_rtp%irank_sph_rtp(1) + 1
      ip_t =  sph_rtp%irank_sph_rtp(2) + 1
      ngrp_r = wk_dgrp%istack_r_gl_ngrp(ip_r)                           &
     &       - wk_dgrp%istack_r_gl_ngrp(ip_r-1)
      ngrp_t = wk_dgrp%istack_t_gl_ngrp(ip_t)                           &
     &       - wk_dgrp%istack_t_gl_ngrp(ip_t-1)
!
      if(ngrp_r .lt. 1) then
        write(*,*) 'SGS_param%ngrp_rave_dynamic', my_rank,              &
     &            ngrp_t, wk_dgrp%nprocs_rt(1)
        write(e_message,*)                                              &
     &       'Set radial groupig more than domain decomposition'
        call calypso_mpi_abort(ierr_SGS, e_message)
      else if(ngrp_r .gt. nlayer_fluid) then
        write(*,*) 'SGS_param%ngrp_rave_dynamic', my_rank,              &
     &            ngrp_r, nlayer_fluid
        write(e_message,*)                                              &
     &       'Set radial groupig less than radial node points'
        call calypso_mpi_abort(ierr_SGS, e_message)
      end if
      if(ngrp_t .lt. 1) then
        write(*,*) 'SGS_param%ngrp_medave_dynamic', my_rank,            &
     &            SGS_param%ngrp_medave_dynamic, wk_dgrp%nprocs_rt(2)
        write(e_message,*)                                              &
     &       'Set meridional groupig more than domain decomposition'
        call calypso_mpi_abort(ierr_SGS, e_message)
      else if(ngrp_t .gt. sph_rtp%nidx_rtp(2)) then
        write(*,*) 'SGS_param%ngrp_medave_dynamic', my_rank,            &
     &            ngrp_t, sph_rtp%nidx_rtp(2)
        write(e_message,*)                                              &
     &       'Set meridional groupig less than meridional node points'
        call calypso_mpi_abort(ierr_SGS, e_message)
      end if
!
      end subroutine check_num_grouping_sph_dynamic
!
! -----------------------------------------------------------------------
!
      end module t_work_sph_dynamic_grping
