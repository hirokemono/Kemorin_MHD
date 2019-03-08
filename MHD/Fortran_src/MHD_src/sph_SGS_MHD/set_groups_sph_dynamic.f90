!set_groups_sph_dynamic.f90
!
!      module set_groups_sph_dynamic
!
!      Written by H. Matsui on Aug., 2018
!
!!      subroutine find_grouping_4_dynamic_model                        &
!!     &         (SGS_param, sph_params, sph_rtp, sph_d_grp)
!
      module set_groups_sph_dynamic
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_groups_sph_dynamic
      use t_work_sph_dynamic_grping
!
      implicit  none
!
      private :: check_num_grouping_sph_dynamic
      private :: set_istack_dynamic_sph_grp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine find_grouping_4_dynamic_model                          &
     &         (SGS_param, sph_params, sph_rtp, sph_d_grp)
!
      use t_SGS_control_parameter
      use t_spheric_parameter
      use t_spheric_rtp_data
      use cal_minmax_and_stacks
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      type(sph_dynamic_model_group), intent(inout) :: sph_d_grp
!
      integer(kind = kint) :: nlayer_fl, max, ntot
      type(make_sph_dynamic_model_grp) :: wk_dgrp1
!
!
      nlayer_fl = num_fluid_layer(sph_params, sph_rtp)
      call alloc_mk_sph_dgrp_flag(nprocs, wk_dgrp1)
!
      call MPI_Allgather                                                &
     &   (sph_rtp%irank_sph_rtp(1), 1, CALYPSO_INTEGER,                 &
     &    wk_dgrp1%irank_list_r, 1, CALYPSO_INTEGER, CALYPSO_COMM,      &
     &    ierr_MPI)
      call MPI_Allgather                                                &
     &   (sph_rtp%irank_sph_rtp(2), 1, CALYPSO_INTEGER,                 &
     &    wk_dgrp1%irank_list_t, 1, CALYPSO_INTEGER,                    &
     &    CALYPSO_COMM, ierr_MPI)
!
      call MPI_Allgather(nlayer_fl, 1, CALYPSO_INTEGER,                 &
     &    wk_dgrp1%nri_pe_list, 1, CALYPSO_INTEGER,                     &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_Allgather(sph_rtp%nidx_rtp(2), 1, CALYPSO_INTEGER,       &
     &    wk_dgrp1%nth_pe_list, 1, CALYPSO_INTEGER,                     &
     &    CALYPSO_COMM, ierr_MPI)
      wk_dgrp1%nprocs_rt(1) = maxval(wk_dgrp1%irank_list_r,1) + 1
      wk_dgrp1%nprocs_rt(2) = maxval(wk_dgrp1%irank_list_t,1) + 1
!
!
      call alloc_mk_sph_dgrp_stack(wk_dgrp1)
!
      call find_num_rt_grid_4_fluid(wk_dgrp1)
      call s_cal_total_and_stacks                                       &
     &   (wk_dgrp1%nprocs_rt(1), wk_dgrp1%nri_1d_list,                  &
     &    (sph_params%nlayer_ICB-1), wk_dgrp1%istack_global_kr, ntot)
!
      call s_cal_total_and_stacks                                       &
     &   (wk_dgrp1%nprocs_rt(2), wk_dgrp1%nth_1d_list,                  &
     &    izero, wk_dgrp1%istack_global_lt, ntot)
!
!
      if(iflag_debug .gt. 0) call ckeck_global_nums_4_dynamic(wk_dgrp1)
!
      call dealloc_mk_sph_dgrp_flag(wk_dgrp1)
!
      if(SGS_param%ngrp_rave_dynamic .eq. sph_rtp%nidx_global_rtp(1))   &
     & then
        call s_cal_total_and_stacks                                     &
     &     (wk_dgrp1%nprocs_rt(1), wk_dgrp1%nri_1d_list,                &
     &      izero, wk_dgrp1%istack_r_gl_ngrp, ntot)
      else
        call count_number_4_smp(wk_dgrp1%nprocs_rt(1),                  &
     &      ione, SGS_param%ngrp_rave_dynamic,                          &
     &      wk_dgrp1%istack_r_gl_ngrp, max)
      end if
!
      if(SGS_param%ngrp_medave_dynamic .eq. sph_rtp%nidx_global_rtp(2)) &
     & then
        call s_cal_total_and_stacks                                     &
     &     (wk_dgrp1%nprocs_rt(2), wk_dgrp1%nth_1d_list,                &
     &      izero, wk_dgrp1%istack_t_gl_ngrp, ntot)
      else
        call count_number_4_smp(wk_dgrp1%nprocs_rt(2),                  &
     &      ione, SGS_param%ngrp_medave_dynamic,                        &
     &      wk_dgrp1%istack_t_gl_ngrp, max)
      end if
!
      call check_num_grouping_sph_dynamic(nlayer_fl,                    &
     &    SGS_param, sph_rtp, wk_dgrp1)
!
      call alloc_mk_sph_istack_dynamic(SGS_param, wk_dgrp1)
      call set_istack_dynamic_sph_grp(wk_dgrp1)
!
      if(iflag_debug .gt. 0) then
        call ckeck_make_dynamic_grp_stacks(wk_dgrp1)
      end if
!
!
      call set_sph_dynamic_num_grp(sph_rtp, wk_dgrp1, sph_d_grp)
      if(i_debug .gt. 0) then
        call check_sph_dynamic_grp_num(sph_d_grp)
      end if
!
      call alloc_sph_dynamic_grp_stack(sph_d_grp)
      call set_sph_dynamic_grp_stack(sph_rtp, wk_dgrp1, sph_d_grp)
!
!
      call dealloc_mk_sph_istack_dynamic(wk_dgrp1)
!
      call alloc_sph_dynamic_grp_item(sph_rtp, sph_d_grp)
      call set_sph_dynamic_grp_item(sph_params, sph_rtp, sph_d_grp)
!
      if(i_debug .gt. 0) then
        call check_sph_dynamic_grp_item(sph_rtp, sph_d_grp)
      end if
!
      end subroutine find_grouping_4_dynamic_model
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_dynamic_num_grp(sph_rtp, wk_dgrp, sph_d_grp)
!
      use t_spheric_rtp_data
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(make_sph_dynamic_model_grp), intent(in) :: wk_dgrp
!
      type(sph_dynamic_model_group), intent(inout) :: sph_d_grp
!
      integer(kind = kint) :: ip
!
!
      ip = sph_rtp%irank_sph_rtp(1) + 1
      sph_d_grp%ngrp_rt(1) = wk_dgrp%istack_r_gl_ngrp(ip)               &
     &                    - wk_dgrp%istack_r_gl_ngrp(ip-1)
!
      ip = sph_rtp%irank_sph_rtp(2) + 1
      sph_d_grp%ngrp_rt(2) = wk_dgrp%istack_t_gl_ngrp(ip)               &
     &                    - wk_dgrp%istack_t_gl_ngrp(ip-1)
!
      sph_d_grp%ngrp_dynamic                                            &
     &        = (sph_d_grp%ngrp_rt(1) + 1) * sph_d_grp%ngrp_rt(2)
!
      end subroutine set_sph_dynamic_num_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_dynamic_grp_stack                              &
     &         (sph_rtp, wk_dgrp, sph_d_grp)
!
      use t_spheric_rtp_data
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(make_sph_dynamic_model_grp), intent(in) :: wk_dgrp
!
      type(sph_dynamic_model_group), intent(inout) :: sph_d_grp
!
      integer(kind = kint) :: ip_r, ip_t, i, ist, ngrp
      integer(kind = kint) :: kr, kst, lt, lst
!
!
      ip_r =  sph_rtp%irank_sph_rtp(1) + 1
      ip_t =  sph_rtp%irank_sph_rtp(2) + 1
      kst =   wk_dgrp%istack_r_gl_ngrp(ip_r-1)
      lst =   wk_dgrp%istack_t_gl_ngrp(ip_t-1)
      do kr = 1, sph_d_grp%ngrp_rt(1)
        do lt = 1, sph_d_grp%ngrp_rt(2)
          i =  kr + (lt-1) * (sph_d_grp%ngrp_rt(1) + 1)
          sph_d_grp%igrp_gl_dynamic(i,1) = kr + kst
          sph_d_grp%igrp_gl_dynamic(i,2) = lt + lst
        end do
      end do
      do lt = 1, sph_d_grp%ngrp_rt(2)
        i =  lt * (sph_d_grp%ngrp_rt(1) + 1)
        sph_d_grp%igrp_gl_dynamic(i,1) = 0
        sph_d_grp%igrp_gl_dynamic(i,2) = lt + lst
      end do
!
      ip_r = sph_rtp%irank_sph_rtp(1) + 1
      ist = wk_dgrp%istack_r_gl_ngrp(ip_r-1)
      ngrp = sph_d_grp%ngrp_rt(1)
      do i = 1, ngrp
        sph_d_grp%istack_dynamic_kr(i)                                  &
     &       = wk_dgrp%istack_rgrp(i+ist) - wk_dgrp%istack_rgrp(ist)
      end do
      sph_d_grp%ntot_dynamic_rt(1) = sph_d_grp%istack_dynamic_kr(ngrp)
!
      ip_t = sph_rtp%irank_sph_rtp(2) + 1
      ist = wk_dgrp%istack_t_gl_ngrp(ip_t-1)
      ngrp = sph_d_grp%ngrp_rt(2)
      do i = 1, ngrp
        sph_d_grp%istack_dynamic_lt(i)                                  &
     &       = wk_dgrp%istack_tgrp(i+ist) - wk_dgrp%istack_tgrp(ist)
      end do
      sph_d_grp%ntot_dynamic_rt(2) = sph_d_grp%istack_dynamic_lt(ngrp)
!
      end subroutine set_sph_dynamic_grp_stack
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_dynamic_grp_item                               &
     &         (sph_params, sph_rtp, sph_d_grp)
!
      use t_spheric_parameter
      use t_spheric_rtp_data
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      type(sph_dynamic_model_group), intent(inout) :: sph_d_grp
!
      integer(kind = kint) :: i, kr, kr_gl, k, kst, ked
      integer(kind = kint) :: lt, lt_gl, l, lst, led
!
!
      i = 0
      do kr = 1, sph_rtp%nidx_rtp(1)
        kr_gl = sph_rtp%idx_gl_1d_rtp_r(kr)
        if(kr_gl .ge. sph_params%nlayer_ICB                             &
     &       .and. kr_gl .le. sph_params%nlayer_CMB) then
          i = i + 1
          sph_d_grp%kr_dynamic(i) =    kr
          sph_d_grp%kr_gl_dynamic(i) = kr_gl
        end if
      end do
!
      i = 0
      do lt = 1, sph_rtp%nidx_rtp(2)
        lt_gl = sph_rtp%idx_gl_1d_rtp_t(lt)
        i = i + 1
        sph_d_grp%lt_dynamic(i) =    lt
        sph_d_grp%lt_gl_dynamic(i) = lt_gl
      end do
!
      sph_d_grp%kgrp_dynamic = sph_d_grp%ngrp_rt(1) + 1
      do i = 1, sph_d_grp%ngrp_rt(1)
        kst = sph_d_grp%istack_dynamic_kr(i-1) + 1
        ked = sph_d_grp%istack_dynamic_kr(i)
        do k = kst, ked
          kr = sph_d_grp%kr_dynamic(k)
          sph_d_grp%kgrp_dynamic(kr) = i
        end do
      end do
!
      do i = 1, sph_d_grp%ngrp_rt(2)
        lst = sph_d_grp%istack_dynamic_lt(i-1) + 1
        led = sph_d_grp%istack_dynamic_lt(i)
        do l = lst, led
          lt = sph_d_grp%lt_dynamic(l)
          sph_d_grp%lgrp_dynamic(lt) = i
        end do
      end do
!
      end subroutine set_sph_dynamic_grp_item
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function num_fluid_layer                     &
     &                            (sph_params, sph_rtp)
!
      use t_spheric_parameter
      use t_spheric_rtp_data
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      integer(kind = kint) :: i, kr, kr_gl
!
!
      i = 0
      do kr = 1, sph_rtp%nidx_rtp(1)
        kr_gl = sph_rtp%idx_gl_1d_rtp_r(kr)
        if(kr_gl .ge. sph_params%nlayer_ICB                             &
     &       .and. kr_gl .le. sph_params%nlayer_CMB) i = i + 1
      end do
      num_fluid_layer = i
!
      end function num_fluid_layer
!
! -----------------------------------------------------------------------
!
      end module set_groups_sph_dynamic
