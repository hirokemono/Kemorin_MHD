!
!     module itp_potential_on_edge
!
!      Written by H. Matsui
!      Modified by H. Matsui on Oct., 2005
!
!!      subroutine cal_potential_on_edge                                &
!!     &         (node, ele, edge, iphys, nod_fld)
!
      module itp_potential_on_edge
!
      use m_precision
!
      use t_geometry_data
      use t_edge_data
      use t_phys_data
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
      subroutine cal_potential_on_edge                                  &
     &         (node, ele, edge, iphys, nod_fld)
!
      use m_geometry_constants
      use m_phys_labels
      use m_control_parameter
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint) :: i
!
      if (ele%nnod_4_ele .eq. num_t_quad) then
!
        do i = 1, nod_fld%num_phys
!
          if     (nod_fld%phys_name(i) .eq. fhd_press) then
            call cal_pressure_on_edge(node%numnod,                      &
     &          edge%numedge, edge%nnod_4_edge, edge%ie_edge,           &
     &          nod_fld%ntot_phys, iphys%i_press, nod_fld%d_fld)
          else if(nod_fld%phys_name(i) .eq. fhd_mag_potential) then
            call cal_pressure_on_edge(node%numnod,                      &
     &          edge%numedge, edge%nnod_4_edge, edge%ie_edge,           &
     &          nod_fld%ntot_phys, iphys%i_mag_p, nod_fld%d_fld)
          else if(nod_fld%phys_name(i) .eq. fhd_scalar_potential) then
            call cal_pressure_on_edge(node%numnod,                      &
     &          edge%numedge, edge%nnod_4_edge, edge%ie_edge,           &
     &          nod_fld%ntot_phys, iphys%i_scalar_p, nod_fld%d_fld)
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
