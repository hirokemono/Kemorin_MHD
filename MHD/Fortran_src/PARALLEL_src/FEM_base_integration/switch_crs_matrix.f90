!
!      module switch_crs_matrix
!
!      Written by H. Matsui
!
!      subroutine s_switch_crs_matrix
!
      module switch_crs_matrix
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
      subroutine s_switch_crs_matrix
!
      use m_geometry_data
      use m_crs_connect
      use m_crs_matrix
!
      integer (kind = kint) :: kk, inod, k1, ii, k2
      real(kind = kreal) :: w
!
!
!
      do inod = 1, node1%numnod
        do k1 = 1, NB_crs
          if ( abs(D_crs(k1,k1,inod)) .eq. 0.0d0) then
            do k2 = k1+1, NB_crs
!                write(*,*) 'switch compornent for ', inod
!                write(*,*) 'original', k1, k2
!                write(*,'(1p20e20.12)') (D_crs(k1,kk,inod),kk=1,NB_crs)
!                 write(*,'(1p20e20.12)') (D_crs(k2,kk,inod),kk=1,NB_crs)
              if ( abs(D_crs(k2,k1,inod)) .ne. 0.0d0 ) then
!                write(*,*) 'switched'
                w = B_crs(NB_crs*(inod-1)+k1)
                B_crs(NB_crs*(inod-1)+k1) = B_crs(NB_crs*(inod-1)+k2)
                B_crs(NB_crs*(inod-1)+k2) = w
                do kk = 1, NB_crs
                  w =                 D_crs(k1,kk,inod)
                  D_crs(k1,kk,inod) = D_crs(k2,kk,inod)
                  D_crs(k2,kk,inod) = w
                end do
                do ii = istack_crs_l(inod-1)+1, istack_crs_l(inod)
                  do kk = 1, NB_crs
                    w =                AL_crs(k1,kk,ii)
                    AL_crs(k1,kk,ii) = AL_crs(k2,kk,ii)
                    AL_crs(k2,kk,ii) = w
                  end do
                end do
                do ii = istack_crs_u(inod-1)+1, istack_crs_u(inod)
                  do kk = 1, NB_crs
                    w =                AU_crs(k1,kk,ii)
                    AU_crs(k1,kk,ii) = AU_crs(k2,kk,ii)
                    AU_crs(k2,kk,ii) = w
                  end do
                end do
                exit
              end if
            end do
          end if
        end do
      end do
!
!
      end subroutine s_switch_crs_matrix
!
!  ---------------------------------------------------------------------
!
      end module switch_crs_matrix
