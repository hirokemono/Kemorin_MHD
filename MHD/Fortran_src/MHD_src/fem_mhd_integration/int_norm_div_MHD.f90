!
!     module int_norm_div_MHD
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine int_norm_div_v_monitor                               &
!!     &         (iloop, node, ele, fluid, iphys, nod_fld,              &
!!     &          g_FEM, jac_3d, j_ave, fem_wk, fem_msq, rsig)
!!      subroutine int_norm_div_b_monitor                               &
!!     &         (iloop, node, ele, iphys, nod_fld, g_FEM, jac_3d,      &
!!     &          j_ave, fem_wk, fem_msq, rsig)
!!      subroutine int_norm_div_a_monitor                               &
!!     &         (iloop, node, ele, iphys, nod_fld, g_FEM, jac_3d,      &
!!     &          j_ave, fem_wk, fem_msq, rsig)
!!
!!      subroutine int_norm_divergence(iele_fsmp_stack, i_field,        &
!!     &          node, ele, nod_fld, g_FEM, jac_3d, fem_wk, res_norm)
!
      module int_norm_div_MHD
!
      use calypso_mpi
      use m_machine_parameter
!
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_finite_element_mat
      use t_mean_square_values
!
      implicit none
!
      integer(kind = kint), parameter, private :: nint_single = 1
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
      subroutine int_norm_div_v_monitor                                 &
     &         (iloop, node, ele, fluid, iphys, nod_fld,                &
     &          g_FEM, jac_3d, j_ave, fem_wk, fem_msq, rsig)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(field_geometry_data), intent(in) :: fluid
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_address), intent(in) :: j_ave
!
      integer(kind = kint), intent(in) :: iloop
!
      real(kind = kreal), intent(inout) :: rsig
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(mean_square_values), intent(inout) :: fem_msq
!
!
      call int_norm_divergence                                          &
     &   (fluid%istack_ele_fld_smp, iphys%i_velo, node, ele, nod_fld,   &
     &    g_FEM, jac_3d, fem_wk, fem_msq%ave_local(j_ave%i_div_v))
      call MPI_allREDUCE                                                &
     &   (fem_msq%ave_local(j_ave%i_div_v) , div_v_sig, ione,           &
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
      subroutine int_norm_div_b_monitor                                 &
     &         (iloop, node, ele, iphys, nod_fld, g_FEM, jac_3d,        &
     &          j_ave, fem_wk, fem_msq, rsig)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_address), intent(in) :: j_ave
!
      integer(kind = kint), intent(in) :: iloop
!
      real(kind = kreal), intent(inout) :: rsig
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(mean_square_values), intent(inout) :: fem_msq
!
!
      call int_norm_divergence                                          &
     &   (ele%istack_ele_smp, iphys%i_magne, node, ele, nod_fld,        &
     &    g_FEM, jac_3d, fem_wk, fem_msq%ave_local(j_ave%i_div_b))
      call MPI_allREDUCE                                                &
     &   (fem_msq%ave_local(j_ave%i_div_b) , div_b_sig, ione,           &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
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
      subroutine int_norm_div_a_monitor                                 &
     &         (iloop, node, ele, iphys, nod_fld, g_FEM, jac_3d,        &
     &          j_ave, fem_wk, fem_msq, rsig)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_address), intent(in) :: j_ave
!
      integer(kind = kint), intent(in) :: iloop
!
      real(kind = kreal), intent(inout) :: rsig
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(mean_square_values), intent(inout) :: fem_msq
!
!
      call int_norm_divergence                                          &
     &   (ele%istack_ele_smp, iphys%i_vecp, node, ele, nod_fld,         &
     &    g_FEM, jac_3d, fem_wk, fem_msq%ave_local(j_ave%i_div_a))
      call MPI_allREDUCE                                                &
     &   (fem_msq%ave_local(j_ave%i_div_a) , div_a_sig, ione,           &
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
     &          node, ele, nod_fld, g_FEM, jac_3d, fem_wk, res_norm)
!
      use fem_skv_div_normal
      use sum_normalized_div
      use nodal_fld_2_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind = kint), intent(in)    :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in)    :: i_field
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      real(kind = kreal), intent(inout) :: res_norm(1)
!
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
     &      np_smp, iele_fsmp_stack, g_FEM%max_int_point,               &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      jac_3d%ntot_int, nint_single, jac_3d%xjac, jac_3d%dnx,      &
     &      k2, fem_wk%vector_1, fem_wk%scalar_1)
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
