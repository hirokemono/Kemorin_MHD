!clear_work_4_dynamic_model.f90
!      module clear_work_4_dynamic_model
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!     subroutine s_clear_work_4_dynamic_model
!
      module clear_work_4_dynamic_model
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_clear_work_4_dynamic_model
!
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_data
      use copy_nodal_fields
!
!
      call clear_nodal_data                                             &
     &   (node1, nod_fld1, n_sym_tensor, iphys%i_sgs_simi)
      call clear_nodal_data                                             &
     &   (node1, nod_fld1, n_sym_tensor, iphys%i_sgs_grad)
      call clear_nodal_data                                             &
     &   (node1, nod_fld1, n_sym_tensor, iphys%i_sgs_grad_f)
!
      end subroutine s_clear_work_4_dynamic_model
!
!  --------------------------------------------------------------------
!
      end module clear_work_4_dynamic_model
