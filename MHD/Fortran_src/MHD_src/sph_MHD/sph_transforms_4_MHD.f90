!sph_transforms_4_MHD.f90
!      module sph_transforms_4_MHD
!
!        programmed by H.Matsui on Oct., 2009
!
!      subroutine init_sph_transform_MHD
!
!      subroutine sph_back_trans_4_MHD
!      subroutine sph_forward_trans_4_MHD
!
!      subroutine sph_back_trans_snapshot_MHD
!      subroutine sph_forward_trans_snapshot_MHD
!
      module sph_transforms_4_MHD
!
      use m_precision
      use m_constants
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_transform_MHD
!
      use m_parallel_var_dof
      use m_machine_parameter
      use m_addresses_trans_sph_MHD
      use m_addresses_trans_sph_snap
      use m_work_4_sph_trans
      use init_sph_trans
!
      real(kind = kreal) :: stime, etime(0:3), etime_shortest
      real(kind = kreal) :: etime_trans(0:3)
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_addresses_trans_sph_MHD'
      call set_addresses_trans_sph_MHD
      call set_addresses_snapshot_trans
!
      if(iflag_debug .gt. 0) call check_add_trans_sph_MHD
      if(iflag_debug .gt. 0) call check_addresses_snapshot_trans
!
      call initialize_sph_trans
!
!
      if(     id_lagendre_transfer .eq. iflag_lag_orginal_loop          &
     &  .and. id_lagendre_transfer .eq. iflag_lag_largest_loop          &
     &  .and. id_lagendre_transfer .eq. iflag_lag_krloop_inner          &
     &  .and. id_lagendre_transfer .eq. iflag_lag_krloop_outer) return
!
      id_lagendre_transfer = iflag_lag_orginal_loop
      stime = MPI_WTIME()
      call sph_back_trans_4_MHD
      call sph_forward_trans_4_MHD
      etime(id_lagendre_transfer) = MPI_WTIME() - stime
!
      id_lagendre_transfer = iflag_lag_largest_loop
      stime = MPI_WTIME()
      call sph_back_trans_4_MHD
      call sph_forward_trans_4_MHD
      etime(id_lagendre_transfer) = MPI_WTIME() - stime
!
      id_lagendre_transfer = iflag_lag_krloop_inner
      stime = MPI_WTIME()
      call sph_back_trans_4_MHD
      call sph_forward_trans_4_MHD
      etime(id_lagendre_transfer) = MPI_WTIME() - stime
!
      id_lagendre_transfer = iflag_lag_krloop_outer
      stime = MPI_WTIME()
      call sph_back_trans_4_MHD
      call sph_forward_trans_4_MHD
      etime(id_lagendre_transfer) = MPI_WTIME() - stime
!
      call MPI_allREDUCE (etime, etime_trans, ifour,                    &
     &    MPI_DOUBLE_PRECISION, MPI_SUM, SOLVER_COMM, ierr)
!
      id_lagendre_transfer = iflag_lag_orginal_loop
      etime_shortest =       etime_trans(iflag_lag_orginal_loop)
!
      if(etime_trans(iflag_lag_largest_loop) .lt. etime_shortest) then
        id_lagendre_transfer = iflag_lag_largest_loop
        etime_shortest =       etime_trans(iflag_lag_largest_loop)
      end if
      if(etime_trans(iflag_lag_krloop_inner) .lt. etime_shortest) then
        id_lagendre_transfer = iflag_lag_krloop_inner
        etime_shortest =       etime_trans(iflag_lag_krloop_inner)
      end if
      if(etime_trans(iflag_lag_krloop_outer) .lt. etime_shortest) then
        id_lagendre_transfer = iflag_lag_krloop_outer
        etime_shortest =       etime_trans(iflag_lag_krloop_outer)
      end if
!
      if(my_rank .eq. 0) then
        write(*,*) 'id_lagendre_transfer: ', id_lagendre_transfer
        write(*,*) '0: elapsed by original loop: ',                     &
     &            etime_trans(iflag_lag_orginal_loop)
        write(*,*) '1: elapsed by longest loop: ',                      &
     &            etime_trans(iflag_lag_largest_loop)
        write(*,*) '2: elapsed by inner radius loop: ',                 &
     &            etime_trans(iflag_lag_krloop_inner)
        write(*,*) '3: elapsed by outer radius loop: ',                 &
     &            etime_trans(iflag_lag_krloop_outer)
      end if
!
      end subroutine init_sph_transform_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_back_trans_4_MHD
!
      use m_addresses_trans_sph_MHD
      use copy_MHD_4_sph_trans
!
      use sph_trans_vector
      use sph_trans_scalar
!
!
      if(nvector_rj_2_rtp .gt. 0) then
        call copy_mhd_vec_spec_to_trans
        call sph_b_trans_vector(nvector_rj_2_rtp)
        call copy_mhd_vec_fld_from_trans
      end if
!
      if(nscalar_rj_2_rtp .gt. 0) then
        call copy_mhd_scl_spec_to_trans
        call sph_b_trans_scalar(nscalar_rj_2_rtp)
        call copy_mhd_scl_fld_from_trans
      end if
!
      end subroutine sph_back_trans_4_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_4_MHD
!
      use m_addresses_trans_sph_MHD
      use copy_MHD_4_sph_trans
!
      use sph_trans_vector
      use sph_trans_scalar
!
!
      if(nvector_rtp_2_rj .gt. 0) then
        call copy_mhd_vec_fld_to_trans
        call sph_f_trans_vector(nvector_rtp_2_rj)
        call copy_mhd_vec_spec_from_trans
      end if
!
      end subroutine sph_forward_trans_4_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_back_trans_snapshot_MHD
!
      use sph_trans_scalar
      use sph_trans_vector
      use copy_snap_4_sph_trans
!
!
!   transform for vectors
      call copy_snap_vec_spec_to_trans
      call sph_b_trans_vector(nvector_snap_rj_2_rtp)
      call copy_snap_vec_fld_from_trans
!
!   transform for scalars
      call copy_snap_scl_spec_to_trans
      call sph_b_trans_scalar(nscalar_snap_rj_2_rtp)
      call copy_snap_scl_fld_from_trans
!
      end subroutine sph_back_trans_snapshot_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_snapshot_MHD
!
      use sph_trans_scalar
      use copy_snap_4_sph_trans
!
!
      call copy_snap_scl_fld_to_trans
      call sph_f_trans_scalar(nscalar_snap_rtp_2_rj)
      call copy_snap_scl_spec_from_trans
!
      end subroutine sph_forward_trans_snapshot_MHD
!
!-----------------------------------------------------------------------
!
      end module sph_transforms_4_MHD
