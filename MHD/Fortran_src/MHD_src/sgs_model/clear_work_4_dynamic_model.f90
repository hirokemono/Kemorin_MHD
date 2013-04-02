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
       use m_geometry_parameter
       use m_node_phys_address
       use m_node_phys_data
!
       integer(kind = kint) :: inod
!
!$omp parallel do
         do inod = 1, numnod
           d_nod(inod,iphys%i_sgs_simi  ) =   0.0d0
           d_nod(inod,iphys%i_sgs_simi+1) =   0.0d0
           d_nod(inod,iphys%i_sgs_simi+2) =   0.0d0
           d_nod(inod,iphys%i_sgs_simi+3) =   0.0d0
           d_nod(inod,iphys%i_sgs_simi+4) =   0.0d0
           d_nod(inod,iphys%i_sgs_simi+5) =   0.0d0
!
           d_nod(inod,iphys%i_sgs_grad  ) =   0.0d0
           d_nod(inod,iphys%i_sgs_grad+1) =   0.0d0
           d_nod(inod,iphys%i_sgs_grad+2) =   0.0d0
           d_nod(inod,iphys%i_sgs_grad+3) =   0.0d0
           d_nod(inod,iphys%i_sgs_grad+4) =   0.0d0
           d_nod(inod,iphys%i_sgs_grad+5) =   0.0d0
!
           d_nod(inod,iphys%i_sgs_grad_f  ) = 0.0d0
           d_nod(inod,iphys%i_sgs_grad_f+1) = 0.0d0
           d_nod(inod,iphys%i_sgs_grad_f+2) = 0.0d0
           d_nod(inod,iphys%i_sgs_grad_f+3) = 0.0d0
           d_nod(inod,iphys%i_sgs_grad_f+4) = 0.0d0
           d_nod(inod,iphys%i_sgs_grad_f+5) = 0.0d0
         end do
!$omp end parallel do
!
       end subroutine s_clear_work_4_dynamic_model
!
!  --------------------------------------------------------------------
!
      end module clear_work_4_dynamic_model
