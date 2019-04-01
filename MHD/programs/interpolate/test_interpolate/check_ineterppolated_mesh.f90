!
!      module check_ineterppolated_mesh
!
!     Written by H. Matsui on Sep., 2006
!
!!      subroutine s_check_ineterppolated_mesh                          &
!!     &         (dest_node, inod_global_itp, xx_interpolate)
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
      subroutine s_check_ineterppolated_mesh                            &
     &         (dest_node, inod_global_itp, xx_interpolate)
!
      use calypso_mpi
      use m_2nd_pallalel_vector
!
      use t_geometry_data
!
      type(node_data), intent(in) :: dest_node
!
      integer(kind = kint_gl), intent(in)                               &
     &               :: inod_global_itp(dest_node%numnod)
      real(kind = kreal), intent(in)                                    &
     &               :: xx_interpolate(dest_node%numnod,3)
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
          do inod = 1, dest_node%numnod
            iflag = inod_global_itp(inod) - dest_node%inod_global(inod)
            rflag                                                       &
     &         = sqrt((xx_interpolate(inod,1)-dest_node%xx(inod,1))**2  &
     &              + (xx_interpolate(inod,2)-dest_node%xx(inod,2))**2  &
     &              + (xx_interpolate(inod,3)-dest_node%xx(inod,3))**2)
!
            if(iflag .gt. 0 .or. rflag.gt.1.0e-11) then
              write(50,*) my_rank, inod, dest_node%inod_global(inod),   &
     &               inod_global_itp(inod), rflag
            end if
          end do
          close(50)
!
        end if
        call calypso_MPI_barrier
      end do
!
      end subroutine s_check_ineterppolated_mesh
!
!------------------------------------------------------------------
!
      end module check_ineterppolated_mesh
