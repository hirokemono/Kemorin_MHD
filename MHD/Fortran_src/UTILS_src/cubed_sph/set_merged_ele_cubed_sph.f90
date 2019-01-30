!set_merged_ele_cubed_sph.f90
!      module set_merged_ele_cubed_sph
!
!     Writetn by H. Matsui on Apr., 2006
!
!      subroutine count_merged_cube(is_level, ifile)
!      subroutine output_domain_4_merge(is_level, ifile)
!      subroutine set_merged_cube_data(is_level, ifile)
!      subroutine set_merge_4_shell(is_level, ifile)
!
      module set_merged_ele_cubed_sph
!
      use m_precision
!
      use m_numref_cubed_sph
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine write_header_4_transfer(is_level, ifile)
!
      integer(kind = kint), intent(in) :: is_level, ifile
!
      write(ifile,'(a)') '!'
      write(ifile,'(a)') '!  coarsing level'
      write(ifile,'(a)') '!'
      write(ifile,'(i16)') is_level
!
      write(ifile,'(a)') '!'
      write(ifile,'(a)') '!  number of internal node and element'
      write(ifile,'(a)') '!'
      write(ifile,'(2i16)') numnod_coarse, numele_coarse
!
      write(ifile,'(a)') '!'
      write(ifile,'(a)') '!  Domain ID local node ID for coarse grid, '
      write(ifile,'(a)') '!  Domain ID local node ID for finer grid,  '
      write(ifile,'(a)') '!  Domain ID local node ID for original grid'
      write(ifile,'(a)') '!'
!
      end subroutine write_header_4_transfer
!
!   --------------------------------------------------------------------
!
      subroutine count_merged_cube(is_level, ifile)
!
      use m_cubed_sph_surf_mesh
!
      integer(kind = kint), intent(in) :: is_level, ifile
!
      integer(kind = kint) :: i
!
!
      write(ifile,'(a)') '!'
      write(ifile,'(a)') '! number of element for marging'
      write(ifile,'(a)') '!'
!
      write(ifile, '(10i16)') (nl_3,i=1,nele_cube_c),                   &
     &                       (nl_shell,i=1,nele_shell_c)
!
      write(ifile,'(a)') '!'
      write(ifile,'(a)') '! belonged finer element ID'
      write(ifile,'(a)') '!'
!
      end subroutine count_merged_cube
!
!   --------------------------------------------------------------------
!
      subroutine output_domain_4_merge(is_level, ifile)
!
      use m_cubed_sph_surf_mesh
!
      integer(kind = kint), intent(in) :: is_level, ifile
!
      integer(kind = kint) :: i, iele
      integer(kind = kint), parameter :: izero = 0
!
!
      write(ifile,'(a)') '!'
      write(ifile,'(a)') '! belonged finer domain ID'
      write(ifile,'(a)') '!'
!
      do iele = 1, nele_cube_c
        write(ifile, '(100i16)') iele, (izero,i=1,nl_3)
      end do
      do iele = nele_cube_c+1, nele_cube_c+nele_shell_c
        write(ifile, '(100i16)') iele, (izero,i=1,nl_shell)
      end do
!
      end subroutine output_domain_4_merge
!
!   --------------------------------------------------------------------
!
      subroutine set_merged_cube_data(is_level, ifile)
!
      use m_cubed_sph_mesh
      use m_cubed_sph_surf_mesh
!
      integer(kind = kint), intent(in) :: is_level, ifile
!
      integer(kind = kint) :: iele0, jele0, kele0
      integer(kind = kint) :: iele, jele, kele
      integer(kind = kint) :: k
      integer(kind = kint) :: ix, iy, iz
      integer(kind = kint) :: jx, jy, jz
!
      character(len=kchara) :: fmt_txt
!
!
      write(*,*) 'imerge_ele', size(c_sphere1%imerge_ele)
      write(fmt_txt,'(a1,i3,a6)')  '(', (nl_3+1), '(i16))'
!
      do iz = 1, n_hemi_c
        do iy = 1, n_hemi_c
          do ix = 1, n_hemi_c
!
            iele0 = nskip_s*(iz-1)*num_hemi**2                          &
     &             + nskip_s*(iy-1)*num_hemi                            &
     &             + nskip_s*(ix-1)
            jele0 = (iz-1)*n_hemi_c**2 + (iy-1)*n_hemi_c + ix-1
            kele0 = nl_s*(iz-1)*n_hemi_fc**2                            &
     &             + nl_s*(iy-1)*n_hemi_fc                              &
     &             + nl_s*(ix-1)
!
            jele = jele0 + 1
!
            do jz = 1, nl_s
              do jy = 1, nl_s
                do jx = 1, nl_s
!
                  k = (jz-1)*nl_s**2 + (jy-1)*nl_s + jx
                  kele = kele0 + (jz-1)*n_hemi_fc**2                    &
     &                         + (jy-1)*n_hemi_fc                       &
     &                         + jx
                  c_sphere1%imerge_ele(k) = kele
!
                end do
              end do
            end do
!
            write(ifile,fmt_txt) jele, c_sphere1%imerge_ele(1:nl_3)
!
          end do
        end do
      end do
!
      end subroutine set_merged_cube_data
!
!   --------------------------------------------------------------------
!
      subroutine set_merge_4_shell(is_level, ifile)
!
      use m_cubed_sph_mesh
      use m_cubed_sph_surf_mesh
      use m_cubed_sph_radius
!
      integer(kind = kint), intent(in) :: is_level, ifile
!
      integer(kind = kint) :: j, k, k0, kk
      integer(kind = kint) :: jele0, kele0, kele1
      integer(kind = kint) :: jele, jele_m
      integer(kind = kint) :: iele_sf, jnum
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a1,i3,a6)')  '(', (nl_shell+1), '(i16))'
      do k = 1, nr_c
        do jnum = 1, nele_sf_c
!
          jele0 = nele_cube_c + (k-1) * nele_sf_c
          kele0 = nele_cube_fc + nl_r*(k-1) * nele_sf_fc
!
          jele = jele0 + jnum
          jele_m = jnum + c_sphere1%iele_stack_sf(is_level-1)
!
          do kk = 1, nl_r
            kele1 = kele0 + (kk-1)*nele_sf_fc
            do j = 1, c_sphere1%num_merge_e_sf(jele_m)
              iele_sf = c_sphere1%imerge_e_sf(jele_m,j)
              k0 = (kk-1) * c_sphere1%num_merge_e_sf(jele_m) + j
              c_sphere1%imerge_ele(k0) = kele1 + iele_sf
            end do
          end do
!
          write(ifile,fmt_txt) jele, c_sphere1%imerge_ele(1:nl_shell)
       end do
      end do
!
      return
      end subroutine set_merge_4_shell
!
!   --------------------------------------------------------------------
!
      end module set_merged_ele_cubed_sph
