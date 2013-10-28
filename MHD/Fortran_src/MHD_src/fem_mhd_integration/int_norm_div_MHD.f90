!
!     module int_norm_div_MHD
!
!     Written by H. Matsui on June, 2005
!
!      subroutine int_norm_div_v_monitor(iloop, rsig)
!      subroutine int_norm_div_b_monitor(iloop, rsig)
!      subroutine int_norm_div_a_monitor(iloop, rsig)
!      subroutine int_norm_div_v
!      subroutine int_norm_div_b
!      subroutine int_norm_div_a
!      subroutine int_norm_div_filter_v
!      subroutine int_norm_div_filter_b
!      subroutine int_norm_div_filter_a
!
      module int_norm_div_MHD
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_control_parameter
      use m_geometry_data
      use m_bulk_values
!
      implicit none
!
      real (kind=kreal) :: div_a_sig, div_a_sig0
      real (kind=kreal) :: div_b_sig, div_b_sig0
      real (kind=kreal) :: div_v_sig, div_v_sig0
!
      private :: int_norm_divergence
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine int_norm_div_v_monitor(iloop, rsig)
!
      use m_node_phys_address
      use m_geometry_data_MHD
!
      integer(kind = kint), intent(in) :: iloop
      real(kind = kreal), intent(inout) :: rsig
!
!
      call int_norm_divergence(iele_fl_smp_stack, ja_divv,              &
     &    iphys%i_velo)
      call MPI_allREDUCE ( bulk_local(ja_divv) , div_v_sig, ione,       &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
!
      div_v_sig = abs(div_v_sig) / vol_fluid
!
      if (div_v_sig .ne. 0.0d0 .and. iloop .ge.0) then
        rsig = ( div_v_sig0-div_v_sig ) / div_v_sig
      end if
      div_v_sig0 = div_v_sig
!
      if (my_rank.eq.0) write(12,*) iloop, ' : <div v> = ', div_v_sig
!
      end subroutine int_norm_div_v_monitor
!
! ----------------------------------------------------------------------
!
      subroutine int_norm_div_b_monitor(iloop, rsig)
!
      use m_node_phys_address
      use m_geometry_parameter
!
      integer(kind = kint), intent(in) :: iloop
      real(kind = kreal), intent(inout) :: rsig
!
!
      call int_norm_divergence(iele_smp_stack, ja_divb, iphys%i_magne)
      call MPI_allREDUCE ( bulk_local(ja_divb) , div_b_sig, ione,       &
     &  CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      div_b_sig = abs(div_b_sig) / volume
!
      if (div_b_sig .ne. 0.0d0 .and. iloop .ge.0) then
        rsig = ( div_b_sig0-div_b_sig ) / div_b_sig
      end if
      div_b_sig0 = div_b_sig
!
      if (my_rank.eq.0) write(12,*) iloop, ' : <div B> = ', div_b_sig
!
      end subroutine int_norm_div_b_monitor
!
! ----------------------------------------------------------------------
!
      subroutine int_norm_div_a_monitor(iloop, rsig)
!
      use m_node_phys_address
      use m_geometry_parameter
!
      integer(kind = kint), intent(in) :: iloop
      real(kind = kreal), intent(inout) :: rsig
!
!
      call int_norm_divergence(iele_smp_stack, ja_diva, iphys%i_vecp)
      call MPI_allREDUCE ( bulk_local(ja_diva) , div_a_sig, ione,       &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      div_a_sig = abs(div_a_sig) / volume
!
      if (div_a_sig .ne. 0.0d0 .and. iloop .ge.0) then
        rsig = ( div_a_sig0-div_a_sig ) / div_a_sig
      end if
      div_a_sig0 = div_a_sig
!
      if (my_rank.eq.0) write(12,*) iloop, ' : <div A> = ', div_a_sig
!
      end subroutine int_norm_div_a_monitor
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_norm_div_v
!
      use m_geometry_data_MHD
      use m_node_phys_address
!
      call int_norm_divergence(iele_fl_smp_stack, ja_divv,              &
     &    iphys%i_velo)
!
      end subroutine int_norm_div_v
!
! ----------------------------------------------------------------------
!
      subroutine int_norm_div_b
!
      use m_geometry_parameter
      use m_node_phys_address
!
!
      call int_norm_divergence(iele_smp_stack, ja_divb, iphys%i_magne)
!
      end subroutine int_norm_div_b
!
! ----------------------------------------------------------------------
!
      subroutine int_norm_div_a
!
      use m_geometry_parameter
      use m_node_phys_address
!
!
      call int_norm_divergence(iele_smp_stack, ja_diva, iphys%i_vecp)
!
      end subroutine int_norm_div_a
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_norm_div_filter_v
!
      use m_geometry_data_MHD
      use m_node_phys_address
!
      call int_norm_divergence(iele_fl_smp_stack, ja_divv_f,            &
     &    iphys%i_filter_velo)
!
      end subroutine int_norm_div_filter_v
!
! ----------------------------------------------------------------------
!
      subroutine int_norm_div_filter_b
!
      use m_geometry_parameter
      use m_node_phys_address
!
!
      call int_norm_divergence(iele_smp_stack, ja_divb_f,               &
     &    iphys%i_filter_magne)
!
      end subroutine int_norm_div_filter_b
!
! ----------------------------------------------------------------------
!
      subroutine int_norm_div_filter_a
!
      use m_geometry_parameter
      use m_node_phys_address
!
!
      call int_norm_divergence(iele_smp_stack, ja_diva_f,               &
     &    iphys%i_filter_vecp)
!
      end subroutine int_norm_div_filter_a
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_norm_divergence(iele_fsmp_stack, j_res, i_field)
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_finite_element_matrix
      use m_int_vol_data
!
      use fem_div_4_norm
      use sum_normalized_div
      use nodal_fld_2_each_ele_1st
!
       integer(kind = kint), intent(in)    :: iele_fsmp_stack(0:np_smp)
       integer(kind = kint), intent(in)    :: i_field
       integer(kind = kint), intent(inout) :: j_res
!
       integer(kind = kint) :: num_int, k2
!
!
      num_int = 1
      phi_e(1:numele) = 0.0d0
!
! -------- loop for shape function for phsical values
!
      do k2=1, nnod_4_ele
!
! ---------  set field at each node in an element
!
        call vector_phys_2_each_element(k2, i_field, vect_e)
        call fem_div_4_norm_pg(iele_fsmp_stack, num_int, k2,            &
     &      vect_e, phi_e)
      end do
!
! --------- caliculate total divergence of velocity
!
      call sum_norm_of_div(numele, np_smp, iele_fsmp_stack,             &
     &    e_multi, phi_e, bulk_local(j_res) )
!
      end subroutine int_norm_divergence
!
! ----------------------------------------------------------------------
!
      end module int_norm_div_MHD
