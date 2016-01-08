!clear_work_4_dynamic_model.f90
!      module clear_work_4_dynamic_model
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!!      subroutine s_clear_work_4_dynamic_model(node, iphys, nod_fld1)
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(inout) :: nod_fld
!
      module clear_work_4_dynamic_model
!
      use m_precision
!
      use m_phys_constants
!
      use t_geometry_data
      use t_phys_address
      use t_phys_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_clear_work_4_dynamic_model(node, iphys, nod_fld)
!
      use copy_nodal_fields
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      call clear_nodal_data                                             &
     &   (node, nod_fld, n_sym_tensor, iphys%i_sgs_simi)
      call clear_nodal_data                                             &
     &   (node, nod_fld, n_sym_tensor, iphys%i_sgs_grad)
      call clear_nodal_data                                             &
     &   (node, nod_fld, n_sym_tensor, iphys%i_sgs_grad_f)
!
      end subroutine s_clear_work_4_dynamic_model
!
!  --------------------------------------------------------------------
!
      end module clear_work_4_dynamic_model
