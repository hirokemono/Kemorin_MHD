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
!
      implicit  none
!
      private :: set_sph_dynamic_istack_global
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
      integer(kind = kint) :: max
      type(make_sph_dynamic_model_grp) :: wk_dgrp1
!
!
      call alloc_mk_sph_dgrp_flag(sph_rtp, wk_dgrp1)
      call count_nprocs_meridional_plans(sph_rtp, wk_dgrp1)
!
      if(iflag_debug .gt. 0) then
        call ckeck_dynamic_grp_iflag(sph_params, sph_rtp, wk_dgrp1)
      end if
!
      call alloc_mk_sph_dgrp_stack(wk_dgrp1)
      call set_sph_dynamic_istack_global                                &
     &         (sph_params, sph_rtp, wk_dgrp1)
      call dealloc_mk_sph_dgrp_flag(wk_dgrp1)
!
      call count_number_4_smp(wk_dgrp1%nprocs_rt(1), ione,              &
     &   SGS_param%ngrp_rave_dynamic, wk_dgrp1%istack_r_gl_ngrp, max)
      call count_number_4_smp(wk_dgrp1%nprocs_rt(2), ione,              &
     &   SGS_param%ngrp_medave_dynamic, wk_dgrp1%istack_t_gl_ngrp, max)
!
!
      call check_num_grouping_sph_dynamic                               &
     &   (SGS_param, sph_params, sph_rtp, wk_dgrp1)
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
      call dealloc_mk_sph_dgrp_stack(wk_dgrp1)
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
! -----------------------------------------------------------------------
!
      subroutine count_nprocs_meridional_plans(sph_rtp, wk_dgrp)
!
      use t_spheric_rtp_data
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(make_sph_dynamic_model_grp), intent(inout) :: wk_dgrp
!
      integer(kind = kint) :: kr, kr_gl, lt, lt_gl
!
!
      if(sph_rtp%irank_sph_rtp(2) .eq. 0                                &
     &      .and. sph_rtp%irank_sph_rtp(3) .eq. 0) then
        do kr = 1, sph_rtp%nidx_rtp(1)
          kr_gl = sph_rtp%idx_gl_1d_rtp_r(kr)
          wk_dgrp%iflag_kr_l(kr_gl) = sph_rtp%irank_sph_rtp(1) + 1
        end do
      end if
!
      call MPI_allREDUCE(wk_dgrp%iflag_kr_l, wk_dgrp%iflag_kr_g,        &
     &    sph_rtp%nidx_global_rtp(1), CALYPSO_INTEGER, MPI_SUM,         &
     &    CALYPSO_COMM, ierr_MPI)
      wk_dgrp%nprocs_rt(1) = maxval(wk_dgrp%iflag_kr_g)
!
      if(sph_rtp%irank_sph_rtp(1) .eq. 0                                &
     &      .and. sph_rtp%irank_sph_rtp(3) .eq. 0) then
        do lt = 1, sph_rtp%nidx_rtp(2)
          lt_gl = sph_rtp%idx_gl_1d_rtp_t(lt)
          wk_dgrp%iflag_lt_l(lt_gl) = sph_rtp%irank_sph_rtp(2) + 1
        end do
      end if
!
      call MPI_allREDUCE(wk_dgrp%iflag_lt_l, wk_dgrp%iflag_lt_g,        &
     &    sph_rtp%nidx_global_rtp(2), CALYPSO_INTEGER, MPI_SUM,         &
     &    CALYPSO_COMM, ierr_MPI)
      wk_dgrp%nprocs_rt(2) = maxval(wk_dgrp%iflag_lt_g)
!
      end subroutine count_nprocs_meridional_plans
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_dynamic_istack_global                          &
     &         (sph_params, sph_rtp, wk_dgrp)
!
      use t_spheric_parameter
      use t_spheric_rtp_data
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(make_sph_dynamic_model_grp), intent(inout) :: wk_dgrp
!
      integer(kind = kint) :: kr, kr_gl, lt, lt_gl
!
!
      wk_dgrp%istack_global_kr(0) = sph_params%nlayer_ICB - 1
      do kr_gl = sph_params%nlayer_ICB, sph_params%nlayer_CMB
        kr = wk_dgrp%iflag_kr_g(kr_gl)
        wk_dgrp%istack_global_kr(kr) = kr_gl
      end do
      do lt_gl = 1, sph_rtp%nidx_global_rtp(2)
        lt = wk_dgrp%iflag_lt_g(lt_gl)
        wk_dgrp%istack_global_lt(lt) = lt_gl
      end do
!
      end subroutine set_sph_dynamic_istack_global
!
! -----------------------------------------------------------------------
!
      subroutine check_num_grouping_sph_dynamic                         &
     &         (SGS_param, sph_params, sph_rtp, wk_dgrp)
!
      use t_SGS_control_parameter
      use t_spheric_parameter
      use t_spheric_rtp_data
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      type(make_sph_dynamic_model_grp), intent(in) :: wk_dgrp
!
      integer(kind = kint) :: nlayer_fluid
      integer(kind = kint) :: ip_r, ip_t, i, kr, kr_gl
      integer(kind = kint) :: nri_fluid, ngrp_r, ngrp_t
!
!
      ip_r =  sph_rtp%irank_sph_rtp(1) + 1
      ip_t =  sph_rtp%irank_sph_rtp(2) + 1
      ngrp_r = wk_dgrp%istack_r_gl_ngrp(ip_r)                           &
     &       - wk_dgrp%istack_r_gl_ngrp(ip_r-1)
      ngrp_t = wk_dgrp%istack_t_gl_ngrp(ip_t)                           &
     &       - wk_dgrp%istack_t_gl_ngrp(ip_t-1)
!
      i = 0
      do kr = 1, sph_rtp%nidx_rtp(1)
        kr_gl = sph_rtp%idx_gl_1d_rtp_r(kr)
        if(kr_gl .ge. sph_params%nlayer_ICB                             &
     &       .and. kr_gl .le. sph_params%nlayer_CMB) i = i + 1
      end do
      nri_fluid = i
!
      nlayer_fluid = sph_params%nlayer_CMB - sph_params%nlayer_ICB + 1
      if(ngrp_r .lt. 1) then
        write(*,*) 'SGS_param%ngrp_rave_dynamic', my_rank,              &
     &            ngrp_t, wk_dgrp%nprocs_rt(1)
        write(e_message,*)                                              &
     &       'Set radial groupig more than domain decomposition'
        call calypso_mpi_abort(1, e_message)
      else if(ngrp_r .gt. nri_fluid) then
        write(*,*) 'SGS_param%ngrp_rave_dynamic', my_rank,              &
     &            ngrp_r, nri_fluid
        write(e_message,*)                                              &
     &       'Set radial groupig less than radial node points'
        call calypso_mpi_abort(1, e_message)
      end if
      if(ngrp_t .lt. 1) then
        write(*,*) 'SGS_param%ngrp_medave_dynamic', my_rank,            &
     &            SGS_param%ngrp_medave_dynamic, wk_dgrp%nprocs_rt(2)
        write(e_message,*)                                              &
     &       'Set meridional groupig more than domain decomposition'
        call calypso_mpi_abort(1, e_message)
      else if(ngrp_t .gt. sph_rtp%nidx_rtp(2)) then
        write(*,*) 'SGS_param%ngrp_medave_dynamic', my_rank,            &
     &            ngrp_t, sph_rtp%nidx_rtp(2)
        write(e_message,*)                                              &
     &       'Set meridional groupig less than meridional node points'
        call calypso_mpi_abort(1, e_message)
      end if
!
      end subroutine check_num_grouping_sph_dynamic
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
!
      end module set_groups_sph_dynamic
