!set_merged_ele_cubed_sph.f90
!      module set_merged_ele_cubed_sph
!
!     Writetn by H. Matsui on Apr., 2006
!
!!      subroutine write_header_4_transfer(is_level, ifile, course_p)
!!      subroutine count_merged_cube(ifile, course_p)
!!      subroutine output_domain_4_merge(ifile, course_p)
!!      subroutine set_merged_cube_data                                 &
!!     &         (ifile, csph_p, course_p, c_sphere)
!!      subroutine set_merge_4_shell                                    &
!!     &         (is_level, ifile, course_p, c_sphere)
!!        type(numref_cubed_sph), intent(in) :: csph_p
!!        type(coarse_cubed_sph), intent(in) :: course_p
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      module set_merged_ele_cubed_sph
!
      use m_precision
      use m_constants
!
      use t_numref_cubed_sph
      use t_cubed_sph_surf_mesh
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine write_header_4_transfer(is_level, ifile, course_p)
!
      integer(kind = kint), intent(in) :: is_level, ifile
      type(coarse_cubed_sph), intent(in) :: course_p
!
      write(ifile,'(a)') '!'
      write(ifile,'(a)') '!  coarsing level'
      write(ifile,'(a)') '!'
      write(ifile,'(i16)') is_level
!
      write(ifile,'(a)') '!'
      write(ifile,'(a)') '!  number of internal node and element'
      write(ifile,'(a)') '!'
      write(ifile,'(2i16)')                                             &
     &            course_p%numnod_coarse, course_p%numele_coarse
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
      subroutine count_merged_cube(ifile, course_p)
!
      integer(kind = kint), intent(in) :: ifile
      type(coarse_cubed_sph), intent(in) :: course_p
!
      integer(kind = kint) :: i
!
!
      write(ifile,'(a)') '!'
      write(ifile,'(a)') '! number of element for marging'
      write(ifile,'(a)') '!'
!
      write(ifile, '(10i16)') (course_p%nl_3,i=1,course_p%nele_cube_c), &
     &           (course_p%nl_shell,i=1,course_p%nele_shell_c)
!
      write(ifile,'(a)') '!'
      write(ifile,'(a)') '! belonged finer element ID'
      write(ifile,'(a)') '!'
!
      end subroutine count_merged_cube
!
!   --------------------------------------------------------------------
!
      subroutine output_domain_4_merge(ifile, course_p)
!
      integer(kind = kint), intent(in) :: ifile
      type(coarse_cubed_sph), intent(in) :: course_p
!
      integer(kind = kint) :: i, iele, ist, ied
!
!
      write(ifile,'(a)') '!'
      write(ifile,'(a)') '! belonged finer domain ID'
      write(ifile,'(a)') '!'
!
      do iele = 1, course_p%nele_cube_c
        write(ifile, '(100i16)') iele, (izero,i=1,course_p%nl_3)
      end do
!
      ist = course_p%nele_cube_c+1
      ied = course_p%nele_cube_c + course_p%nele_shell_c
      do iele = ist, ied
        write(ifile, '(100i16)') iele, (izero,i=1,course_p%nl_shell)
      end do
!
      end subroutine output_domain_4_merge
!
!   --------------------------------------------------------------------
!
      subroutine set_merged_cube_data                                   &
     &         (ifile, csph_p, course_p, c_sphere)
!
      integer(kind = kint), intent(in) :: ifile
      type(numref_cubed_sph), intent(in) :: csph_p
      type(coarse_cubed_sph), intent(in) :: course_p
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: iele0, jele0, kele0
      integer(kind = kint) :: jele, kele
      integer(kind = kint) :: k
      integer(kind = kint) :: ix, iy, iz
      integer(kind = kint) :: jx, jy, jz
!
      character(len=kchara) :: fmt_txt
!
!
      write(*,*) 'imerge_ele', size(c_sphere%imerge_ele)
      write(fmt_txt,'(a1,i3,a6)')  '(', (course_p%nl_3+1), '(i16))'
!
      do iz = 1, course_p%n_hemi_c
        do iy = 1, course_p%n_hemi_c
          do ix = 1, course_p%n_hemi_c
!
            iele0 = course_p%nskip_s*(iz-1)*csph_p%num_hemi**2          &
     &             + course_p%nskip_s*(iy-1)*csph_p%num_hemi            &
     &             + course_p%nskip_s*(ix-1)
            jele0 = (iz-1)*course_p%n_hemi_c**2                         &
     &             + (iy-1)*course_p%n_hemi_c + ix-1
            kele0 = course_p%nl_s*(iz-1)*course_p%n_hemi_fc**2          &
     &             + course_p%nl_s*(iy-1)*course_p%n_hemi_fc            &
     &             + course_p%nl_s*(ix-1)
!
            jele = jele0 + 1
!
            do jz = 1, course_p%nl_s
              do jy = 1, course_p%nl_s
                do jx = 1, course_p%nl_s
!
                  k = (jz-1)*course_p%nl_s**2                           &
     &               + (jy-1)*course_p%nl_s + jx
                  kele = kele0 + (jz-1)*course_p%n_hemi_fc**2           &
     &                         + (jy-1)*course_p%n_hemi_fc              &
     &                         + jx
                  c_sphere%imerge_ele(k) = kele
!
                end do
              end do
            end do
!
            write(ifile,fmt_txt) jele,                                  &
     &                          c_sphere%imerge_ele(1:course_p%nl_3)
          end do
        end do
      end do
!
      end subroutine set_merged_cube_data
!
!   --------------------------------------------------------------------
!
      subroutine set_merge_4_shell                                      &
     &         (is_level, ifile, course_p, c_sphere)
!
      integer(kind = kint), intent(in) :: is_level, ifile
      type(coarse_cubed_sph), intent(in) :: course_p
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: j, k, k0, kk
      integer(kind = kint) :: jele0, kele0, kele1
      integer(kind = kint) :: jele, jele_m
      integer(kind = kint) :: iele_sf, jnum
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a1,i3,a6)')  '(', (course_p%nl_shell+1), '(i16))'
      do k = 1, course_p%nr_c
        do jnum = 1, course_p%nele_sf_c
!
          jele0 = course_p%nele_cube_c + (k-1) * course_p%nele_sf_c
          kele0 = course_p%nele_cube_fc                                 &
     &           + course_p%nl_r*(k-1) * course_p%nele_sf_fc
!
          jele = jele0 + jnum
          jele_m = jnum + c_sphere%iele_stack_sf(is_level-1)
!
          do kk = 1, course_p%nl_r
            kele1 = kele0 + (kk-1) * course_p%nele_sf_fc
            do j = 1, c_sphere%num_merge_e_sf(jele_m)
              iele_sf = c_sphere%imerge_e_sf(jele_m,j)
              k0 = (kk-1) * c_sphere%num_merge_e_sf(jele_m) + j
              c_sphere%imerge_ele(k0) = kele1 + iele_sf
            end do
          end do
!
          write(ifile,fmt_txt) jele,                                    &
     &                        c_sphere%imerge_ele(1:course_p%nl_shell)
       end do
      end do
!
      return
      end subroutine set_merge_4_shell
!
!   --------------------------------------------------------------------
!
      end module set_merged_ele_cubed_sph
