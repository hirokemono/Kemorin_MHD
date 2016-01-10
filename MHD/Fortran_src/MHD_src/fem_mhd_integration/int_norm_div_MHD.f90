!
!     module int_norm_div_MHD
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine int_norm_div_v_monitor(iloop, node, ele, fluid,      &
!!     &          iphys, nod_fld, jac_3d, fem_wk, rsig)
!!      subroutine int_norm_div_b_monitor(iloop, node, ele,             &
!!     &          iphys, nod_fld, jac_3d, fem_wk, rsig)
!!      subroutine int_norm_div_a_monitor(iloop, node, ele,             &
!!     &          iphys, nod_fld, jac_3d, fem_wk, rsig)
!!
!!      subroutine int_norm_divergence(iele_fsmp_stack, i_field,        &
!!     &          node, ele, nod_fld, jac_3d, fem_wk, res_norm)
!
      module int_norm_div_MHD
!
      use calypso_mpi
      use m_machine_parameter
      use m_control_parameter
      use m_bulk_values
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
      real (kind=kreal) :: div_a_sig, div_a_sig0
      real (kind=kreal) :: div_b_sig, div_b_sig0
      real (kind=kreal) :: div_v_sig, div_v_sig0
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine int_norm_div_v_monitor(iloop, node, ele, fluid,        &
     &          iphys, nod_fld, jac_3d, fem_wk, rsig)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(field_geometry_data), intent(in) :: fluid
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind = kint), intent(in) :: iloop
!
      real(kind = kreal), intent(inout) :: rsig
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call int_norm_divergence(fluid%istack_ele_fld_smp, iphys%i_velo,  &
     &    node, ele, nod_fld, jac_3d, fem_wk, bulk_local(ja_divv))
      call MPI_allREDUCE ( bulk_local(ja_divv) , div_v_sig, ione,       &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
!
      div_v_sig = abs(div_v_sig) / fluid%volume
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
      subroutine int_norm_div_b_monitor(iloop, node, ele,               &
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
      call int_norm_divergence(ele%istack_ele_smp, iphys%i_magne,       &
     &    node, ele, nod_fld, jac_3d, fem_wk, bulk_local(ja_divb))
      call MPI_allREDUCE ( bulk_local(ja_divb) , div_b_sig, ione,       &
     &  CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      div_b_sig = abs(div_b_sig) / ele%volume
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
      subroutine int_norm_div_a_monitor(iloop, node, ele,               &
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
      call int_norm_divergence(ele%istack_ele_smp, iphys%i_vecp,        &
     &    node, ele, nod_fld, jac_3d, fem_wk, bulk_local(ja_diva))
      call MPI_allREDUCE ( bulk_local(ja_diva) , div_a_sig, ione,       &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      div_a_sig = abs(div_a_sig) / ele%volume
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
      subroutine int_norm_divergence(iele_fsmp_stack, i_field,          &
     &          node, ele, nod_fld, jac_3d, fem_wk, res_norm)
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
      real(kind = kreal), intent(inout) :: res_norm(1)
!
      integer(kind = kint), parameter :: num_int = 1
      integer(kind = kint) :: k2
!
!
!$omp workshare
      fem_wk%scalar_1(1:ele%numele) = 0.0d0
!$omp end workshare
!
! -------- loop for shape function for phsical values
!
      do k2=1, ele%nnod_4_ele
!
! ---------  set field at each node in an element
!
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_field, fem_wk%vector_1)
        call fem_skv_div_normal_pg(ele%numele, ele%nnod_4_ele,          &
     &      np_smp, iele_fsmp_stack, jac_3d%ntot_int, num_int,          &
     &      jac_3d%xjac, jac_3d%dnx, k2, fem_wk%vector_1,               &
     &      fem_wk%scalar_1)
      end do
!
! --------- caliculate total divergence of velocity
!
      call sum_norm_of_div(ele%numele, np_smp, iele_fsmp_stack,        &
     &    ele%interior_ele, fem_wk%scalar_1, res_norm)
!
      end subroutine int_norm_divergence
!
! ----------------------------------------------------------------------
!
      end module int_norm_div_MHD
