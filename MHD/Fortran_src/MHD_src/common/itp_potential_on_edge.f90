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
      use m_node_phys_address
      use m_node_phys_data
      use m_control_parameter
!
      integer(kind = kint) :: i
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
!
        do i = 1, num_nod_phys
!
          if ( phys_nod_name(i) .eq. 'pressure' ) then
            call cal_pressure_on_edge(edge1%numedge, edge1%nnod_4_edge, &
     &          edge1%ie_edge, iphys%i_press)
          else if ( phys_nod_name(i) .eq. 'magnetic_potential' ) then
            call cal_pressure_on_edge(edge1%numedge, edge1%nnod_4_edge, &
     &          edge1%ie_edge, iphys%i_mag_p)
          else if ( phys_nod_name(i) .eq. 'scalar_potential' ) then
            call cal_pressure_on_edge(edge1%numedge, edge1%nnod_4_edge, &
     &          edge1%ie_edge, iphys%i_scalar_p)
          end if
!
        end do
      end if
!
      end subroutine cal_potential_on_edge
!
!-----------------------------------------------------------------------
!
!    interpolate data on tri-linear to quadrature
!
      subroutine cal_pressure_on_edge                                   &
     &         (numedge, nnod_4_edge, ie_edge, i_phys)
!
      use m_constants
      use m_node_phys_address
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: i_phys
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
