!
!     module read_positions_of_spectr
!
!      Written by H.Matsui
!      Modified by H.Matsui on June, 2007
!
!      subroutine s_read_positions_of_spectr(iz_max, zz)
!
      module read_positions_of_spectr
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
      subroutine s_read_positions_of_spectr(iz_max, zz)
!
      use set_spectr_file_name
!
      integer (kind = kint), intent(in) :: iz_max
      real(kind = kreal), intent(inout) :: zz(iz_max)
!
      integer (kind = kint) :: spectr_data_code = 22
      integer (kind = kint) :: ix, iy, iz, itmp
!
      open (spectr_data_code,  file=spec_mode_file_name,                &
     &                       form='formatted',  status ='unknown')
!
      read(spectr_data_code,*) 
      do iz = 1, iz_max
         read(spectr_data_code,*) ix, iy, itmp, zz(iz)
      end do
!
      close(spectr_data_code)
!
      end subroutine s_read_positions_of_spectr
!
!  ---------------------------------------------------------------------
!
      end module read_positions_of_spectr
