!
!     module average_on_elements
!
!      Written by H.Matsui
!      Moified by H. Matsui on Sep., 2007
!
!!      subroutine vector_on_element_1st                                &
!!     &         (node, ele, g_FEM, jac_3d, iele_fsmp_stack, n_int,     &
!!     &          ifld_nod, nod_fld, ifld_ele, ele_fld)
!!      subroutine rotation_on_element_1st                              &
!!     &         (node, ele, g_FEM, jac_3d, iele_fsmp_stack, n_int,     &
!!     &          ifld_nod, nod_fld, ifld_ele, ele_fld)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(phys_data), intent(in) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!
      module average_on_elements
!
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_phys_data
!
      use cal_fields_on_element
      use cal_differences_on_ele
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine vector_on_element_1st                                  &
     &         (node, ele, g_FEM, jac_3d, iele_fsmp_stack, n_int,       &
     &          ifld_nod, nod_fld, ifld_ele, ele_fld)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
      integer(kind = kint), intent(in) :: ifld_nod, ifld_ele
      type(phys_data), intent(in) :: nod_fld
!
      type(phys_data), intent(inout) :: ele_fld
!
!
      call vector_on_element                                            &
     &   (node, ele, jac_3d, iele_fsmp_stack,                    &
     &    n_int, nod_fld%d_fld(1,ifld_nod), ele_fld%d_fld(1,ifld_ele))
      ele_fld%iflag_update(ifld_ele:ifld_ele+2) = 1
!
      end subroutine vector_on_element_1st
!
! -----------------------------------------------------------------------
!
      subroutine rotation_on_element_1st                                &
     &         (node, ele, g_FEM, jac_3d, iele_fsmp_stack, n_int,       &
     &          ifld_nod, nod_fld, ifld_ele, ele_fld)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
      integer(kind = kint), intent(in) :: ifld_nod, ifld_ele
      type(phys_data), intent(in) :: nod_fld
!
      type(phys_data), intent(inout) :: ele_fld
!
!
      call rotation_on_element                                          &
     &   (node, ele, g_FEM, jac_3d, iele_fsmp_stack, n_int,             &
     &    nod_fld%d_fld(1,ifld_nod), ele_fld%d_fld(1,ifld_ele))
      ele_fld%iflag_update(ifld_ele:ifld_ele+2) = 1
!
      end subroutine rotation_on_element_1st
!
! -----------------------------------------------------------------------
!
      end module average_on_elements
