!
!      module set_field_on_edge
!
!      Written by H. Matsui on Feb., 2007
!
!      subroutine set_scalar_on_edge_quad(numnod, numedge,              &
!     &          nnod_4_edge, iedge_smp_stack, ie_edge,                 &
!     &          num_tot_nod_phys, i_field, d_nod)
!      subroutine set_vector_on_edge_quad(numnod, numedge,              &
!     &          nnod_4_edge, iedge_smp_stack, ie_edge,                 &
!     &          num_tot_nod_phys, i_field, d_nod)
!      subroutine set_sym_tensor_on_edge_quad(numnod, numedge,          &
!     &          nnod_4_edge, iedge_smp_stack, ie_edge,                 &
!     &          num_tot_nod_phys, i_field, d_nod)
!
!      subroutine cal_scalar_field_on_edge(numnod, numedge,             &
!     &          nnod_4_edge, iedge_smp_stack, ie_edge,                 &
!     &          num_tot_nod_phys, ntot_edge_phys, i_field,             &
!     &          d_nod, d_edge)
!      subroutine cal_vector_field_on_edge(numnod, numedge,             &
!     &          nnod_4_edge, iedge_smp_stack, ie_edge,                 &
!     &          num_tot_nod_phys, ntot_edge_phys, i_field,             &
!     &          d_nod, d_edge)
!      subroutine cal_tensor_field_on_edge(numnod, numedge,             &
!     &          nnod_4_edge, iedge_smp_stack, ie_edge,                 &
!     &          num_tot_nod_phys, ntot_edge_phys, i_field,             &
!     &          d_nod, d_edge)
!
      module set_field_on_edge
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_scalar_on_edge_quad(numnod, numedge,               &
     &          nnod_4_edge, iedge_smp_stack, ie_edge,                  &
     &          num_tot_nod_phys, i_field, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: iedge_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: num_tot_nod_phys
      real(kind=kreal), intent(inout) :: d_nod(numnod,num_tot_nod_phys)
!
      integer(kind = kint) :: ip, ist, ied, iedge, inod, i1, i3
!
!
!$omp parallel do private(ist,ied,iedge,inod,i1,i3)
      do ip = 1, np_smp
        ist = iedge_smp_stack(ip-1)
        ied = iedge_smp_stack(ip)
        do iedge = ist, ied
          inod = ie_edge(iedge,2)
          i1 = ie_edge(iedge,1)
          i3 = ie_edge(iedge,3)
          d_nod(inod,i_field)                                           &
     &          = half * ( d_nod(i1,i_field) + d_nod(i3,i_field) )
        end do
      end do
!$omp end parallel do
!
      end subroutine set_scalar_on_edge_quad
!
! -----------------------------------------------------------------------
!
      subroutine set_vector_on_edge_quad(numnod, numedge,               &
     &          nnod_4_edge, iedge_smp_stack, ie_edge,                  &
     &          num_tot_nod_phys, i_field, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: iedge_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: num_tot_nod_phys
      real(kind=kreal), intent(inout) :: d_nod(numnod,num_tot_nod_phys)
!
      integer(kind = kint) :: ip, ist, ied, iedge, inod, i1, i3
!
!
!$omp parallel do private(ist,ied,iedge,inod,i1,i3)
      do ip = 1, np_smp
        ist = iedge_smp_stack(ip-1)
        ied = iedge_smp_stack(ip)
        do iedge = ist, ied
          inod = ie_edge(iedge,2)
          i1 = ie_edge(iedge,1)
          i3 = ie_edge(iedge,3)
          d_nod(inod,i_field  )                                         &
     &          = half * ( d_nod(i1,i_field  ) + d_nod(i3,i_field  ) )
          d_nod(inod,i_field+1)                                         &
     &          = half * ( d_nod(i1,i_field+1) + d_nod(i3,i_field+1) )
          d_nod(inod,i_field+2)                                         &
     &          = half * ( d_nod(i1,i_field+2) + d_nod(i3,i_field+2) )
        end do
      end do
!$omp end parallel do
!
      end subroutine set_vector_on_edge_quad
!
! -----------------------------------------------------------------------
!
      subroutine set_sym_tensor_on_edge_quad(numnod, numedge,           &
     &          nnod_4_edge, iedge_smp_stack, ie_edge,                  &
     &          num_tot_nod_phys, i_field, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: iedge_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: num_tot_nod_phys
      real(kind=kreal), intent(inout) :: d_nod(numnod,num_tot_nod_phys)
!
      integer(kind = kint) :: ip, ist, ied, iedge, inod, i1, i3
!
!
!$omp parallel do private(ist,ied,iedge,inod,i1,i3)
      do ip = 1, np_smp
        ist = iedge_smp_stack(ip-1)
        ied = iedge_smp_stack(ip)
        do iedge = ist, ied
          inod = ie_edge(iedge,2)
          i1 = ie_edge(iedge,1)
          i3 = ie_edge(iedge,3)
          d_nod(inod,i_field  )                                         &
     &          = half * ( d_nod(i1,i_field  ) + d_nod(i3,i_field  ) )
          d_nod(inod,i_field+1)                                         &
     &          = half * ( d_nod(i1,i_field+1) + d_nod(i3,i_field+1) )
          d_nod(inod,i_field+2)                                         &
     &          = half * ( d_nod(i1,i_field+2) + d_nod(i3,i_field+2) )
          d_nod(inod,i_field+3)                                         &
     &          = half * ( d_nod(i1,i_field+3) + d_nod(i3,i_field+3) )
          d_nod(inod,i_field+4)                                         &
     &          = half * ( d_nod(i1,i_field+4) + d_nod(i3,i_field+4) )
          d_nod(inod,i_field+5)                                         &
     &          = half * ( d_nod(i1,i_field+5) + d_nod(i3,i_field+5) )
        end do
      end do
!$omp end parallel do
!
      end subroutine set_sym_tensor_on_edge_quad
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_scalar_field_on_edge(numnod, numedge,              &
     &          nnod_4_edge, iedge_smp_stack, ie_edge,                  &
     &          num_tot_nod_phys, ntot_edge_phys, i_field,              &
     &          d_nod, d_edge)
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: iedge_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: num_tot_nod_phys
      integer(kind = kint), intent(in) :: ntot_edge_phys
      real(kind=kreal), intent(in) ::    d_nod(numnod,num_tot_nod_phys)
      real(kind=kreal), intent(inout) :: d_edge(numedge,ntot_edge_phys)
!
      integer(kind = kint) :: ip, ist, ied
      integer(kind = kint) :: iedge, inod1, inod2
!
!
!$omp parallel do private(ip,ist,ied,iedge,inod1,inod2)
      do ip = 1, np_smp
        ist = iedge_smp_stack(ip-1) + 1
        ied = iedge_smp_stack(ip)
        do iedge = ist, ied
!
          inod1 = ie_edge(iedge,1)
          inod2 = ie_edge(iedge,2)
          d_edge(iedge,i_field  ) = half * (d_nod(inod1,i_field  )      &
     &                                   +  d_nod(inod2,i_field  ) )
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_scalar_field_on_edge
!
!-----------------------------------------------------------------------
!
      subroutine cal_vector_field_on_edge(numnod, numedge,              &
     &          nnod_4_edge, iedge_smp_stack, ie_edge,                  &
     &          num_tot_nod_phys, ntot_edge_phys, i_field,              &
     &          d_nod, d_edge)
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: iedge_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: num_tot_nod_phys
      integer(kind = kint), intent(in) :: ntot_edge_phys
      real(kind=kreal), intent(in) ::    d_nod(numnod,num_tot_nod_phys)
      real(kind=kreal), intent(inout) :: d_edge(numedge,ntot_edge_phys)
!
      integer(kind = kint) :: ip, ist, ied
      integer(kind = kint) :: iedge, inod1, inod2
!
!
!$omp parallel do private(ip,ist,ied,iedge,inod1,inod2)
      do ip = 1, np_smp
        ist = iedge_smp_stack(ip-1) + 1
        ied = iedge_smp_stack(ip)
        do iedge = ist, ied
!
          inod1 = ie_edge(iedge,1)
          inod2 = ie_edge(iedge,2)
          d_edge(iedge,i_field  ) = half * (d_nod(inod1,i_field  )      &
     &                                   +  d_nod(inod2,i_field  ) )
          d_edge(iedge,i_field  ) = half * (d_nod(inod1,i_field+1)      &
     &                                   +  d_nod(inod2,i_field+1) )
          d_edge(iedge,i_field  ) = half * (d_nod(inod1,i_field+2)      &
     &                                   +  d_nod(inod2,i_field+2) )
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_vector_field_on_edge
!
!-----------------------------------------------------------------------
!
      subroutine cal_tensor_field_on_edge(numnod, numedge,              &
     &          nnod_4_edge, iedge_smp_stack, ie_edge,                  &
     &          num_tot_nod_phys, ntot_edge_phys, i_field,              &
     &          d_nod, d_edge)
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: iedge_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: num_tot_nod_phys
      integer(kind = kint), intent(in) :: ntot_edge_phys
      real(kind=kreal), intent(in) ::    d_nod(numnod,num_tot_nod_phys)
      real(kind=kreal), intent(inout) :: d_edge(numedge,ntot_edge_phys)
!
      integer(kind = kint) :: ip, ist, ied
      integer(kind = kint) :: iedge, inod1, inod2
!
!
!$omp parallel do private(ip,ist,ied,iedge,inod1,inod2)
      do ip = 1, np_smp
        ist = iedge_smp_stack(ip-1) + 1
        ied = iedge_smp_stack(ip)
        do iedge = ist, ied
!
          inod1 = ie_edge(iedge,1)
          inod2 = ie_edge(iedge,2)
          d_edge(iedge,i_field  ) = half * (d_nod(inod1,i_field  )      &
     &                                   +  d_nod(inod2,i_field  ) )
          d_edge(iedge,i_field+1) = half * (d_nod(inod1,i_field+1)      &
     &                                   +  d_nod(inod2,i_field+1) )
          d_edge(iedge,i_field+2) = half * (d_nod(inod1,i_field+2)      &
     &                                   +  d_nod(inod2,i_field+2) )
          d_edge(iedge,i_field+3) = half * (d_nod(inod1,i_field+3)      &
     &                                   +  d_nod(inod2,i_field+3) )
          d_edge(iedge,i_field+4) = half * (d_nod(inod1,i_field+4)      &
     &                                   +  d_nod(inod2,i_field+4) )
          d_edge(iedge,i_field+5) = half * (d_nod(inod1,i_field+5)      &
     &                                   +  d_nod(inod2,i_field+5) )
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_tensor_field_on_edge
!
!-----------------------------------------------------------------------
!
      end module set_field_on_edge
