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
      subroutine s_switch_crs_matrix(tbl_crs, mat_crs)
!
      use t_crs_connect
      use t_crs_matrix
!
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(CRS_matrix), intent(inout) :: mat_crs
!
      integer (kind = kint) :: kk, inod, k1, ii, k2, ist, ied, NB
      integer (kind = kint) :: k1k, k2k, i1i, i2i
      real(kind = kreal) :: w
!
!
!
      NB = mat_crs%NB_crs
      do inod = 1, tbl_crs%ntot_d
        do k1 = 1, NB
          i1i = k1 + (k1-1) * NB + (inod-1) * NB*NB
          if ( abs(mat_crs%D_crs(i1i)) .eq. 0.0d0) then
            do k2 = k1+1, NB
              i2i = k2 + (k1-1) * NB + (inod-1) * NB*NB
              if ( abs(mat_crs%D_crs(i2i)) .ne. 0.0d0 ) then
                w = mat_crs%B_crs(NB*(inod-1)+k1)
                mat_crs%B_crs(NB*(inod-1)+k1)                           &
     &            = mat_crs%B_crs(NB*(inod-1)+k2)
                mat_crs%B_crs(NB*(inod-1)+k2) = w
                do kk = 1, NB
                  k1k = k1 + (kk-1) * NB + (inod-1) * NB*NB
                  k2k = k1 + (kk-1) * NB + (inod-1) * NB*NB
                  w = mat_crs%D_crs(k1k)
                  mat_crs%D_crs(k1k) = mat_crs%D_crs(k2k)
                  mat_crs%D_crs(k2k) = w
                end do
!
                ist = tbl_crs%istack_l(inod-1)+1
                ied = tbl_crs%istack_l(inod)
                do ii = ist, ied
                  do kk = 1, NB
                    k1k = k1 + (kk-1)*NB + (ii-1)*NB*NB
                    k2k = k2 + (kk-1)*NB + (ii-1)*NB*NB
                    w = mat_crs%AL_crs(k1k)
                    mat_crs%AL_crs(k1k) = mat_crs%AL_crs(k2k)
                    mat_crs%AL_crs(k2k) = w
                  end do
                end do
!
                ist = tbl_crs%istack_u(inod-1)+1
                ied = tbl_crs%istack_u(inod)
                do ii = ist, ied
                  do kk = 1, NB
                    k1k = k1 + (kk-1)*NB + (ii-1)*NB*NB
                    w = mat_crs%AU_crs(k1k)
                    mat_crs%AU_crs(k1k) = mat_crs%AU_crs(k2k)
                    mat_crs%AU_crs(k2k) = w
                  end do
                end do
                exit
              end if
            end do
          end if
        end do
      end do
!
      end subroutine s_switch_crs_matrix
!
!  ---------------------------------------------------------------------
!
      end module switch_crs_matrix
