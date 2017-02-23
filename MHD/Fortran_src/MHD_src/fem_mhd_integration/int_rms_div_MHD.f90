!
!     module int_rms_div_MHD
!
!     Written by H. Matsui on June, 2005
!
!      subroutine int_rms_div_v_monitor(iloop, rsig)
!      subroutine int_rms_div_b_monitor(iloop, rsig)
!      subroutine int_rms_div_a_monitor(iloop, rsig)
!      subroutine int_rms_div_v
!      subroutine int_rms_div_b
!      subroutine int_rms_div_a
!      subroutine int_rms_div_filter_v
!      subroutine int_rms_div_filter_b
!      subroutine int_rms_div_filter_a
!
      module int_rms_div_MHD
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_mean_square_values
!
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_jacobian_3d
      use t_finite_element_mat
      use t_finite_element_mat
!
      implicit none
!
      real (kind=kreal) :: rms_div_a_sig,  rms_div_a_sig0
      real (kind=kreal) :: rms_div_b_sig,  rms_div_b_sig0
      real (kind=kreal) :: rms_div_v_sig,  rms_div_v_sig0
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine int_rms_div_v_monitor(iloop, node, ele, fluid,         &
     &         iphys, nod_fld, jac_3d, fem_wk, rsig)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind = kint), intent(in) :: iloop
      real(kind = kreal), intent(inout) :: rsig
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call int_rms_divergence                                           &
     &   (fluid%istack_ele_fld_smp, iphys%i_velo,                       &
     &    node, ele, nod_fld, jac_3d, fem_wk, rms_local(ir_divv))
!
      call MPI_allREDUCE (rms_local(ir_divv) , rms_div_v_sig, 1,        &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      rms_div_v_sig = sqrt(rms_div_v_sig / fluid%volume)
!
      if (rms_div_v_sig .ne. 0.0d0 .and. iloop .ge.0) then
        rsig = ( rms_div_v_sig0-rms_div_v_sig ) / rms_div_v_sig
      end if
      rms_div_v_sig0 = rms_div_v_sig
!
      if (my_rank.eq.0)                                                 &
     &  write(12,*) iloop, ' : RMS(div v) = ', rms_div_v_sig0
!
      end subroutine int_rms_div_v_monitor
!
! ----------------------------------------------------------------------
!
      subroutine int_rms_div_b_monitor(iloop, node, ele,                &
     &          iphys, nod_fld, jac_3d, fem_wk, rsig)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind = kint), intent(in) :: iloop
      real(kind = kreal), intent(inout) :: rsig
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call int_rms_divergence(ele%istack_ele_smp, iphys%i_magne,        &
     &    node, ele, nod_fld, jac_3d, fem_wk, rms_local(ir_divb))
!
      call MPI_allREDUCE (rms_local(ir_divb) , rms_div_b_sig, 1,        &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      rms_div_b_sig = sqrt(rms_div_b_sig / ele%volume)
!
      if (rms_div_b_sig .ne. 0.0d0 .and. iloop .ge.0) then
        rsig = ( rms_div_b_sig0-rms_div_b_sig ) / rms_div_b_sig
      end if
      rms_div_b_sig0 = rms_div_b_sig
!
      if (my_rank.eq.0)                                                 &
     &  write(12,*) iloop, ' : RMS(div B) = ', rms_div_b_sig0
!
!
      end subroutine int_rms_div_b_monitor
!
! ----------------------------------------------------------------------
!
      subroutine int_rms_div_a_monitor(iloop, node, ele,                &
     &          iphys, nod_fld, jac_3d, fem_wk, rsig)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind = kint), intent(in) :: iloop
!
      real(kind = kreal), intent(inout) :: rsig
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call int_rms_divergence(ele%istack_ele_smp, iphys%i_vecp,         &
     &    node, ele, nod_fld, jac_3d, fem_wk, rms_local(ir_diva))
!
      call MPI_allREDUCE ( rms_local(ir_diva) , rms_div_a_sig, 1,       &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      rms_div_a_sig = sqrt(rms_div_a_sig / ele%volume)
!
      if (rms_div_a_sig .ne. 0.0d0 .and. iloop .ge.0) then
        rsig = ( rms_div_a_sig0-rms_div_a_sig ) / rms_div_a_sig
      end if
      rms_div_a_sig0 = rms_div_a_sig
!
      if (my_rank.eq.0)                                                 &
     &  write(12,*) iloop, ' : RMS(div A) = ', rms_div_a_sig0
!
!
      end subroutine int_rms_div_a_monitor
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_rms_divergence(iele_fsmp_stack, i_field,           &
     &          node, ele, nod_fld, jac_3d, fem_wk, res_sq)
!
      use fem_skv_div_normal
      use sum_normalized_div
      use nodal_fld_2_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind = kint), intent(in)    :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in)    :: i_field
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      real(kind = kreal), intent(inout) :: res_sq(1)
!
      integer(kind = kint), parameter :: num_int = 1
      integer(kind = kint) ::  k2
!
!
!$omp workshare
      fem_wk%scalar_1(1:ele%numele) =  0.0d0
!$omp end workshare
!
! -------- loop for shape function for phsical values
!
      do k2 = 1, ele%nnod_4_ele
!
! ---------  set vector at each node in an element
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &     k2, i_field, fem_wk%vector_1)
        call fem_skv_rms_flux_pg(ele%numele, ele%nnod_4_ele,            &
     &      np_smp, iele_fsmp_stack, jac_3d%ntot_int, num_int,          &
     &      jac_3d%xjac, jac_3d%dnx, k2, fem_wk%vector_1,               &
     &      fem_wk%scalar_1)
      end do
!
! --------- caliculate total divergence of velocity
!
      call sum_norm_of_div(ele%numele, np_smp, iele_fsmp_stack,         &
     &    ele%interior_ele, fem_wk%scalar_1, res_sq(1))
!
      end subroutine int_rms_divergence
!
! ----------------------------------------------------------------------
!
      end module int_rms_div_MHD
