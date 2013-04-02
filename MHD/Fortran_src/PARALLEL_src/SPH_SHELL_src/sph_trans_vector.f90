!
!      module sph_trans_vector
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine sph_b_trans_vector(nb)
!      subroutine sph_f_trans_vector(nb)
!
!   input /outpt arrays
!
!      radial component:      vr_rtp(3*i_rtp-2)
!      elevetional component: vr_rtp(3*i_rtp-1)
!      azimuthal component:   vr_rtp(3*i_rtp  )
!
!     forward transform: 
!      Poloidal component:          sp_rj(3*i_rj-2)
!      diff. of Poloidal component: sp_rj(3*i_rj-1)
!      Toroidal component:          sp_rj(3*i_rj  )
!
!     backward transform: 
!      Poloidal component:          sp_rj(3*i_rj-2)
!      diff. of Poloidal component: sp_rj(3*i_rj-1)
!      Toroidal component:          sp_rj(3*i_rj  )
!
!
!
!      subroutine sph_b_trans_grad_v(nb)
!      subroutine sph_f_trans_grad_v(nb)
!
!   input /outpt arrays
!
!     forward transform: 
!     backward transform: 
!
!      radial component:      vr_rtp(3*i_rtp-2)
!      elevetional component: vr_rtp(3*i_rtp-1)
!      azimuthal component:   vr_rtp(3*i_rtp  )
!      Poloidal component:          sp_rj(3*i_rj-2)
!      diff. of Poloidal component: sp_rj(3*i_rj-1)
!      Toroidal component:          sp_rj(3*i_rj  )
!
      module sph_trans_vector
!
      use m_precision
!
      use m_parallel_var_dof
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use FFT_selector
      use schmidt_trans_vector
      use schmidt_trans_vector_org
      use schmidt_trans_vector_krin
      use schmidt_trans_vector_spin
      use schmidt_trans_grad_v
      use schmidt_trans_grad_org
      use schmidt_trans_grad_krin
      use schmidt_trans_grad_spin
      use merge_polidal_toroidal_v
      use spherical_SRs_N
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_vector(nb)
!
!      use m_work_time
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: Nstacksmp(0:np_smp)
      integer(kind = kint) :: np, nb3, ncomp
!
!
      nb3 = 3*nb
      np =    nidx_rtp(3)
      ncomp = 3*nb*nidx_rtp(1)*nidx_rtp(2)
      Nstacksmp(0:np_smp) = 3*nb*irt_rtp_smp_stack(0:np_smp)
!
!      call check_sp_rj(my_rank, nb3)
!
      START_TIME= MPI_WTIME()
      call send_recv_rj_2_rlm_N(nb3, sp_rj, sp_rlm)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
!      call check_sp_rlm(my_rank, nb3)
!
!      call start_eleps_time(18)
      if(id_lagendre_transfer .eq. iflag_lag_krloop_outer) then
        if(iflag_debug .gt. 0) write(*,*) 'schmidt_b_trans_vector_spin'
        call schmidt_b_trans_vector_spin(nb)
      else if(id_lagendre_transfer .eq. iflag_lag_krloop_inner) then
        if(iflag_debug .gt. 0) write(*,*) 'schmidt_b_trans_vector_krin'
        call schmidt_b_trans_vector_krin(nb)
      else
        if(iflag_debug .gt. 0) write(*,*) 'schmidt_b_trans_vector'
        call schmidt_b_trans_vector(nb)
      end if
!      call end_eleps_time(18)
!      call schmidt_b_trans_vector_org(nb)
!
!      call check_vr_rtm(my_rank, nb3)
!
      START_TIME= MPI_WTIME()
      call send_recv_rtm_2_rtp_N(nb3, vr_rtm, vr_rtp)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
!      call check_vr_rtp(my_rank, nb3 )
!
      call backward_FFT_select(np_smp, Nstacksmp, ncomp, np, vr_rtp)
!
!      call check_vr_rtp(my_rank, nb3 )
!
      call const_vect_sph_b_trans(nb, vr_rtp)
!
      end subroutine sph_b_trans_vector
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_vector(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: Nstacksmp(0:np_smp)
      integer(kind = kint) :: np, nb3, ncomp
!
!
      nb3 = 3*nb
      np =    nidx_rtp(3)
      ncomp = 3*nb*nidx_rtp(1)*nidx_rtp(2)
      Nstacksmp(0:np_smp) = 3*nb*irt_rtp_smp_stack(0:np_smp)
!
      call prod_r_vect_sph_f_trans(nb, vr_rtp)
!
!
!      call check_vr_rtp(my_rank, nb3 )
      call forward_FFT_select(np_smp, Nstacksmp, ncomp, np, vr_rtp)
!      call check_vr_rtp(my_rank, nb3 )
!
      START_TIME= MPI_WTIME()
      call send_recv_rtp_2_rtm_N(nb3, vr_rtp, vr_rtm)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!      call check_vr_rtm(my_rank, nb3)
!
      if(id_lagendre_transfer .eq. iflag_lag_krloop_outer) then
        if(iflag_debug .gt. 0) write(*,*) 'schmidt_f_trans_vector_spin'
        call schmidt_f_trans_vector_spin(nb)
      else if(id_lagendre_transfer .eq. iflag_lag_krloop_inner) then
        if(iflag_debug .gt. 0) write(*,*) 'schmidt_f_trans_vector_krin'
        call schmidt_f_trans_vector_krin(nb)
      else
        if(iflag_debug .gt. 0) write(*,*) 'schmidt_f_trans_vector'
        call schmidt_f_trans_vector(nb)
      end if
!      call schmidt_f_trans_vector_org(nb)
!      call check_sp_rlm(my_rank, nb3)
!
      START_TIME= MPI_WTIME()
      call send_recv_rlm_2_rj_N(nb3, sp_rlm, sp_rj)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!      call check_sp_rj(my_rank, nb3)
!
      end subroutine sph_f_trans_vector
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_grad_v(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: Nstacksmp(0:np_smp)
      integer(kind = kint) :: np, ncomp, nb3, nb2
!
!
      nb2 = 2*nb
      nb3 = 3*nb
      np =    nidx_rtp(3)
      ncomp = 3*nb*nidx_rtp(1)*nidx_rtp(2)
      Nstacksmp(0:np_smp) = 3*nb*irt_rtp_smp_stack(0:np_smp)
!
      START_TIME= MPI_WTIME()
      call send_recv_rj_2_rlm_N(nb2, sp_rj(1), sp_rlm(1) )
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
      if(id_lagendre_transfer .eq. iflag_lag_krloop_outer) then
        call schmidt_b_trans_grad_spin(nb)
      else if(id_lagendre_transfer .eq. iflag_lag_krloop_inner) then
        call schmidt_b_trans_grad_krin(nb)
      else
        call schmidt_b_trans_grad_v(nb)
      end if
!      call schmidt_b_trans_grad_org(nb)
!
      call send_recv_rtm_2_rtp_N(nb3, vr_rtm, vr_rtp)
!
      call backward_FFT_select(np_smp, Nstacksmp, ncomp, np, vr_rtp )
!
      call const_grad_v_sph_b_trans(nb, vr_rtp)
!
      end subroutine sph_b_trans_grad_v
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_grad_v(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: Nstacksmp(0:np_smp)
      integer(kind = kint) :: np, ncomp, nb3
!
!
      call prod_r_grad_v_sph_f_trans(nb, vr_rtp)
!
      np =    nidx_rtp(3)
      nb3 = 3*nb
      ncomp = 3*nb*nidx_rtp(1)*nidx_rtp(2)
      Nstacksmp(0:np_smp) = 3*nb*irt_rtp_smp_stack(0:np_smp)
!
      call forward_FFT_select(np_smp, Nstacksmp, ncomp, np, vr_rtp )
!
      START_TIME= MPI_WTIME()
      call send_recv_rtp_2_rtm_N(nb3, vr_rtp, vr_rtm)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
      if(id_lagendre_transfer .eq. iflag_lag_krloop_outer) then
        call schmidt_f_trans_grad_spin(nb)
      else if(id_lagendre_transfer .eq. iflag_lag_krloop_inner) then
        call schmidt_f_trans_grad_krin(nb)
      else
        call schmidt_f_trans_grad_v(nb)
      end if
!      call schmidt_f_trans_grad_org(nb)
!
      ncomp = 2*nb*nidx_rtp(1)*nidx_rtp(2)
      call send_recv_rlm_2_rj_N(ncomp, sp_rlm(1), sp_rj(1) )
!
      end subroutine sph_f_trans_grad_v
!
! -----------------------------------------------------------------------
!
      end module sph_trans_vector
