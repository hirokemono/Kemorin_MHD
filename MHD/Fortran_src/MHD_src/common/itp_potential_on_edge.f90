!
!     module itp_potential_on_edge
!
!      Written by H. Matsui
!      Modified by H. Matsui on Oct., 2005
!
!      subroutine cal_potential_on_edge
!
      module itp_potential_on_edge
!
      use m_precision
!
      implicit none
!
      private :: cal_pressure_on_edge
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_potential_on_edge
!
      use m_geometry_constants
      use m_geometry_data
      use m_phys_labels
      use m_node_phys_address
      use m_node_phys_data
      use m_control_parameter
!
      integer(kind = kint) :: i
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
!
        do i = 1, nod_fld1%num_phys
!
          if     (nod_fld1%phys_name(i) .eq. fhd_press) then
            call cal_pressure_on_edge(node1%numnod,                     &
     &          edge1%numedge, edge1%nnod_4_edge, edge1%ie_edge,        &
     &          nod_fld1%ntot_phys, iphys%i_press, d_nod)
          else if(nod_fld1%phys_name(i) .eq. fhd_mag_potential) then
            call cal_pressure_on_edge(node1%numnod,                     &
     &          edge1%numedge, edge1%nnod_4_edge, edge1%ie_edge,        &
     &          nod_fld1%ntot_phys, iphys%i_mag_p, d_nod)
          else if(nod_fld1%phys_name(i) .eq. fhd_scalar_potential) then
            call cal_pressure_on_edge(node1%numnod,                     &
     &          edge1%numedge, edge1%nnod_4_edge, edge1%ie_edge,        &
     &          nod_fld1%ntot_phys, iphys%i_scalar_p, d_nod)
          end if
!
        end do
      end if
!
      end subroutine cal_potential_on_edge
!
!-----------------------------------------------------------------------
!
      subroutine cal_pressure_on_edge                                   &
     &         (numnod, numedge, nnod_4_edge, ie_edge,                  &
     &          ncomp_nod, i_phys, d_nod)
!
      use m_constants
!
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_phys
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: i, i1,  i2,  i3
!
!
!$omp parallel do private(i,i1,i2,i3)
       do i = 1, numedge
        i1  = ie_edge(i, 1)
        i2  = ie_edge(i, 2)
        i3  = ie_edge(i, 3)
!
        d_nod(i2,i_phys) = half * (d_nod(i1,i_phys) + d_nod(i3,i_phys))
      end do
!$omp end parallel do
!
      end subroutine cal_pressure_on_edge
!
!-----------------------------------------------------------------------
!
      end module itp_potential_on_edge
