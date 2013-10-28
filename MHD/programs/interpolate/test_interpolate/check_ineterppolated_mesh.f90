!
!      module check_ineterppolated_mesh
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_check_ineterppolated_mesh(my_rank)
!
      module check_ineterppolated_mesh
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_check_ineterppolated_mesh
!
      use calypso_mpi
      use m_2nd_pallalel_vector
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_interpolated_geometry
!
      integer(kind = kint) :: ip, inod, iflag
      real(kind = kreal) :: rflag
!
!
      do ip = 1, nprocs_2nd
        if(my_rank .eq. (ip-1) ) then
!
          if(ip .eq. 1) then
            open(50,file='failed_interpolate.dat')
            write(50,*) 'error at'
            write(50,*) 'rank, inod_lc, inod_gl, distance'
          else
            open(50,file='failed_interpolate.dat',position='append')
          end if
!
          do inod = 1, nnod_2nd
            iflag = inod_global_itp(inod) - globalnodid_2nd(inod)
            rflag = sqrt( (xx_interpolate(inod,1) - xx_2nd(inod,1))**2  &
     &                  + (xx_interpolate(inod,2) - xx_2nd(inod,2))**2  &
     &                  + (xx_interpolate(inod,3) - xx_2nd(inod,3))**2)
!
            if(iflag .gt. 0 .or. rflag.gt.1.0e-11) then
              write(50,*) my_rank, inod, globalnodid_2nd(inod),         &
     &               inod_global_itp(inod), rflag
            end if
          end do
          close(50)
!
        end if
        call calypso_MPI_barrier
      end do
!
      call deallocate_interpolate_geometry
!
      end subroutine s_check_ineterppolated_mesh
!
!------------------------------------------------------------------
!
      end module check_ineterppolated_mesh
